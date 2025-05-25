#include <inttypes.h>
#include <math.h>
#include <float.h>

#if _WIN32
#   define WIN32_LEAN_AND_MEAN
#   include <windows.h>
#else
#   include <dlfcn.h>
#endif

_Noreturn static void fatal(const char* msg, ...) {
    va_list args;
    va_start(args, msg);

    vfprintf(stderr, msg, args);
    fputs("\n", stderr);
    fflush(stderr);
    
    va_end(args);
    
    abort();
}

_Noreturn static void Raise(STRING_STRUCT* msg_str) {
    if (msg_str && msg_str->rc.strong_count != 0) {
        int32_t msg_len = STRING_LEN(msg_str);
        char* msg_chars = (char*) STRING_CHARS(msg_str);

        fprintf(stderr, "Runtime error raised: %.*s\n", (int) msg_len, msg_chars);
        fflush(stderr);
    }
    abort();
}

static int32_t System_StrToInt(STRING_STRUCT* str) {
    if (!str || str->rc.strong_count == 0) {
        fprintf(stderr, "called StrToInt for an invalid string pointer\n");
        fflush(stderr);
        abort();
    }

    int i = atoi((char*) STRING_CHARS(str));
    return (int32_t) i;
}

#define INT_TO_STR_IMPL(FuncName, DataType, FormatString, BufSize) \
static STRING_STRUCT* FuncName(DataType i) { \
    char buf[(BufSize)]; \
    sprintf_s(buf, (BufSize), "%" FormatString, i); \
    \
    size_t len = strlen(buf); \
    unsigned char* chars = Alloc(len); \
    memcpy(chars, buf, len); \
    \
    STRING_STRUCT* str = (STRING_STRUCT*) RcAlloc(&STRING_CLASS, false); \
    STRING_LEN(str) = len; \
    STRING_CHARS(str) = chars; \
    \
    return str; \
}
 
INT_TO_STR_IMPL(System_ByteToStr,       uint8_t,        PRIu8,  4  + 0)
INT_TO_STR_IMPL(System_Int8ToStr,       int8_t,         PRId8,  4  + 1)
INT_TO_STR_IMPL(System_UInt16ToStr,     uint16_t,       PRIu16, 8  + 0)
INT_TO_STR_IMPL(System_Int16ToStr,      int16_t,        PRId16, 8  + 1)
INT_TO_STR_IMPL(System_UInt32ToStr,     uint32_t,       PRIu32, 12 + 0)
INT_TO_STR_IMPL(System_IntToStr,        int32_t,        PRId32, 12 + 1)
INT_TO_STR_IMPL(System_UInt64ToStr,     uint64_t,       PRIu64, 24 + 0)
INT_TO_STR_IMPL(System_Int64ToStr,      int64_t,        PRId64, 24 + 1)
INT_TO_STR_IMPL(System_NativeUIntToStr, size_t,         "zu",   24 + 0)
INT_TO_STR_IMPL(System_NativeIntToStr,  ptrdiff_t,      "zd",   24 + 1)
INT_TO_STR_IMPL(System_PointerToStr,    const void*,    "p",    24 + 0)
INT_TO_STR_IMPL(System_RealToStr,       float,          "f",    3 + FLT_MANT_DIG - FLT_MIN_EXP)

static void* Alloc(size_t len) {
    void* mem = calloc((size_t) len, 1);
    if (!mem) {
        fatal("allocation failure");
    }

#ifdef TRACE_HEAP
    struct AllocTrace* new_alloc = malloc(sizeof(struct AllocTrace));
    new_alloc->next = alloc_traces;
    new_alloc->len = len;
    new_alloc->at = mem;
    alloc_traces = new_alloc;

    fprintf(stderr, "heap: alloc %4zu bytes at 0x%p\n", len, mem);
#endif

    return mem;
}

static void Free(void* mem) {
#ifdef TRACE_HEAP
    struct AllocTrace** alloc = &alloc_traces;

    while (*alloc) {
        if ((*alloc)->at == mem) {
            struct AllocTrace* removed = *alloc;
            *alloc = removed->next;

            fprintf(stderr, "heap:  free %4zu bytes at 0x%p\n", removed->len, removed->at);
            free(removed);
            break;
        } else {
            alloc = &((*alloc)->next);
        }
    }
#endif

    free(mem);
}

// RC runtime functions

static void* RcAlloc(struct Class* class, bool immortal) {
    if (!class) {
        abort();
    }

    void* instance = Alloc(class->size);

    // the rc field should be the first member of any rc type
    struct Rc* rc = (struct Rc*)instance;
    rc->class = class;
    rc->strong_count = 1;
    rc->weak_count = 0;

    return rc;
}

static void RcRetain(void* instance, bool weak) {
    if (!instance) {
        return;
    }

    struct Rc* rc = (struct Rc*)instance;
    
    // don't retain immortal refs
    if (rc->strong_count < 0) {
        return;
    }
    
    if (weak) {
        rc->weak_count += 1;
    } else {
        if (rc->strong_count == 0) {
            fatal("resurrecting with 0 strong refs pointer @ 0x%p (+ %d weak refs remain)", instance, rc->weak_count);
        }
    
        rc->strong_count += 1;
    }

#if TRACE_RC
    // safe to use the name chars ptr as a C string here, we null-terminate string literals
    printf("rc: retain %s @ 0x%p (%d+%d refs)\n", 
        TYPEINFO_NAME_CHARS(rc->class->typeinfo), 
        instance, 
        rc->strong_count, 
        rc->weak_count);
#endif
}

static void RcRelease(void* instance, bool weak) {
    if (!instance) {
        // releasing NULL should be ignored
        return;
    }

    struct Rc* rc = (struct Rc*)instance;

    if (rc->strong_count < 0) {
        // immortal
        return;
    }
   
    if (weak) {
        if (rc->weak_count == 0) {
            fatal("releasing with 0 weak refs remaining @ 0x%p", instance);
        }

#if TRACE_RC
        printf("rc: release %s @ 0x%p (%d+%d remain)\n", TYPEINFO_NAME_CHARS(rc->class->typeinfo), instance, rc->strong_count, rc->weak_count - 1);
#endif
        
        rc->weak_count -= 1;
    } else {
        if (rc->strong_count == 0) {
            fatal("releasing with 0 strong refs remaining @ 0x%p", instance);
        }

#if TRACE_RC
        printf("rc: release %s @ 0x%p (%d+%d remain)\n", TYPEINFO_NAME_CHARS(rc->class->typeinfo), instance, rc->strong_count - 1, rc->weak_count);
#endif

        // call the dtor before decrementing the ref count, because it must still be a live reference
        // while the function is executing
        if (rc->strong_count == 1 && rc->class->dtor) {
#if TRACE_RC
            printf("rc: \tdisposing %s @ 0x%p\n", TYPEINFO_NAME_CHARS(rc->class->typeinfo), instance);
#endif
            rc->class->dtor(instance);
            
            // invoke structural release to release struct fields
            if (rc->class->cleanup) {
                rc->class->cleanup(instance);
            }
            rc->class = NULL;

            if (rc->strong_count != 1) {
                fprintf(stderr, "destructor for %s modified the reference count of the destroyed instance\n", TYPEINFO_NAME_CHARS(rc->class->typeinfo));
                fflush(stderr);
                abort();
            }
        }
  
        rc->strong_count -= 1;
    }

    if (rc->strong_count == 0 && rc->weak_count == 0) {
        // free memory
        Free(instance);
    }
}

static unsigned char* System_GetMem(int32_t len) {
    return (unsigned char*) Alloc(len);
}

static void System_FreeMem(unsigned char* mem) {
    Free(mem);
}

static void System_Write(STRING_STRUCT* str) {
    if (!str || str->rc.strong_count == 0) {
        fatal("called Write for an invalid string pointer");
    }

    int len = (int) STRING_LEN(str);
    char* chars = (char*) STRING_CHARS(str);

    printf("%.*s", len, chars);
}

static void System_WriteLn(STRING_STRUCT* str) {
    System_Write(str);
    putchar('\n');
    fflush(stdout);
}

static STRING_STRUCT* System_ReadLn(void) {
    char buf[64];
    if (!fgets(buf, 64, stdin)) {
        fatal("ReadLn i/o failure");
    }

    size_t len = strlen(buf);
    STRING_STRUCT* str = (STRING_STRUCT*) RcAlloc(&STRING_CLASS, false);
    STRING_LEN(str) = (int32_t) len;
    STRING_CHARS(str) = System_GetMem(len);
    memcpy(STRING_CHARS(str), buf, len);

    return str;
}

static int32_t System_ArrayLengthInternal(void* arr) {
    struct Rc* arr_rc = (struct Rc*) arr;
    if (!arr_rc || arr_rc->strong_count == 0) {
        fatal("called Length for an invalid array pointer");
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr_rc->class;

    return array_class->length(arr_rc);
}

static void* System_ArraySetLengthInternal(
    void* arr,
    int32_t new_len,
    void* default_val
) {
    struct Rc* arr_rc = (struct Rc*) arr;
    if (!arr_rc || arr_rc->strong_count == 0) {
        fatal("called SetLength for an invalid array pointer");
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr_rc->class;

    void* new_arr = RcAlloc(arr_rc->class, false);
    array_class->alloc(new_arr, new_len, arr_rc, default_val);

    return new_arr;
}

static void* LoadSymbol(const char* src, const char* sym) {
    void* sym_ptr = NULL;
#if _WIN32
    HINSTANCE lib = LoadLibraryA(src);
    if (lib) {
        sym_ptr = GetProcAddress(lib, sym);
    }
#else
    void* lib = dlopen(src, RTLD_LAZY);
    if (lib) {
        sym_ptr = dlsym(lib, sym);
    }
#endif

    if (!sym_ptr) {
        fatal("failed to load symbol: %s::%s", src, sym);
    }

    return sym_ptr;
}

static int32_t System_RandomInteger(int32_t from, int32_t to) {
    return rand() % (to + 1 - from) + from;
}

static float System_RandomSingle(float from, float to) {
    float range = (float) rand() / (float) RAND_MAX;
    range *= (to - from);

    return from + range;
}

static float System_Pow(float val, float pow) {
    return powf(val, pow);
}

static float System_Sqrt(float val) {
    return sqrtf(val);
}

static float System_Sin(float val) {
    return sinf(val);
}

static float System_ArcSin(float val) {
    return asinf(val);
}

static float System_Cos(float val) {
    return cosf(val);
}

static float System_ArcCos(float val) {
    return acosf(val);
}

static float System_Tan(float val) {
    return tanf(val);
}

static float System_ArcTan(float val) {
    return atanf(val);
}

static float System_Infinity(void) {
    return INFINITY;
}

static bool System_IsInfinite(float val) {
    return isinf(val);
}

static float System_NaN(void) {
    return NAN;
}

static bool System_IsNaN(float val) {
    return isnan(val);
}

static void InvokeMethod(METHODINFO_STRUCT* method, void* instance, POINTERARRAY_STRUCT* args, void* outResult) {
    Invoker invoker = METHODINFO_INVOKER(method);
    if (!invoker) {
        fatal("InvokeMethod called for abstract method");
    }

    if (instance) {
        int args_len = DYNARRAY_LEN(args);

        void** all_args = (void**) malloc(sizeof(void*) * (args_len + 1));
        all_args[0] = instance;

        memcpy(all_args + 1, DYNARRAY_PTR(args), args_len * sizeof(void*));

        invoker(all_args, outResult);

        free(all_args);
    } else {
        invoker(DYNARRAY_PTR(args), outResult);
    }    
}

static TYPEINFO_STRUCT* System_FindTypeInfo(STRING_STRUCT* type_name) {
    if (STRING_LEN(type_name) == 0) {
        return NULL;
    }

    const char* type_name_cstr = (const char*) STRING_CHARS(type_name);

    for (int i = 0; i < typeinfo_count; i += 1) {
        TYPEINFO_STRUCT* item = typeinfo_list[i];
        
        if (!TYPEINFO_NAME(item)
            || STRING_LEN(TYPEINFO_NAME(item)) == 0) {
            continue;
        }
        
        const char* item_name_cstr = (const char*) STRING_CHARS(TYPEINFO_NAME(item));

        size_t cmp_len = (size_t)(min(STRING_LEN(type_name), STRING_LEN(TYPEINFO_NAME(item))));
        if (strncmp(type_name_cstr, item_name_cstr, cmp_len) == 0) {
            return item;
        }
    }
    
    return NULL;
}

static int32_t System_GetTypeInfoCount(void) {
    return typeinfo_count;
}

static TYPEINFO_STRUCT* System_GetTypeInfoByIndex(int32_t type_index) {
    if (type_index < 0 || type_index >= typeinfo_count) {
        return NULL;
    }
    
    return typeinfo_list[type_index];
}

static TYPEINFO_STRUCT* System_GetObjectTypeInfo(struct Rc* object) {
    return object->class->typeinfo;
}

static FUNCINFO_STRUCT* System_FindFunctionInfo(STRING_STRUCT* func_name) {
    if (STRING_LEN(func_name) == 0) {
        return NULL;
    }

    const char* func_name_cstr = (const char*) STRING_CHARS(func_name);

    for (int i = 0; i < funcinfo_count; i += 1) {
        FUNCINFO_STRUCT* item = funcinfo_list[i];
        
        if (!FUNCINFO_NAME(item)
            || STRING_LEN(FUNCINFO_NAME(item)) == 0) {
            continue;
        }
        
        const char* item_name_cstr = (const char*) STRING_CHARS(FUNCINFO_NAME(item));

        size_t cmp_len = (size_t)(min(STRING_LEN(func_name), STRING_LEN(FUNCINFO_NAME(item))));
        if (strncmp(func_name_cstr, item_name_cstr, cmp_len) == 0) {
            return item;
        }
    }
    
    return NULL;
}

static int32_t System_GetFunctionInfoCount(void) {
    return funcinfo_count;
}

static FUNCINFO_STRUCT* System_GetFunctionInfoByIndex(int32_t func_index) {
    if (func_index < 0 || func_index >= funcinfo_count) {
        return NULL;
    }
    
    return funcinfo_list[func_index];
}

static void InvokeFunction(FUNCINFO_STRUCT* func, POINTERARRAY_STRUCT* args, void* outResult) {
    Invoker invoker = FUNCINFO_INVOKER(func);
    if (!invoker) {
        fatal("InvokeFunction called for non-invokable func");
    }

    invoker(DYNARRAY_PTR(args), outResult);
}
