#include <inttypes.h>
#include <math.h>
#include <float.h>
#include <time.h>

#if _WIN32
#   define WIN32_LEAN_AND_MEAN
#   include <windows.h>
#   define STACKALLOC(size) (_malloca(size))
#else
#   include <dlfcn.h>
#   include <alloca.h>
#   define STACKALLOC(size) (alloca(size))
#endif

#ifdef DISABLE_RTTI
#define OBJECT_DISPLAY(instance) "object"
#else
#define OBJECT_DISPLAY(instance) TYPEINFO_NAME_CHARS(((OBJECT_PTR) instance)->class->typeinfo)
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
    STRING_STRUCT* str = (STRING_STRUCT*) RcNew(&STRING_CLASS, false); \
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

static OBJECT_PTR RcNew(struct Class* class, bool immortal) {
    if (!class) {
        abort();
    }

    char* mem = (char*) Alloc(class->size);

    for (size_t i = 0; i < class->size; i += 1) {
        mem[i] = 0;
    }
    
    OBJECT_PTR instance = (OBJECT_PTR) mem;

    instance->class = class;
    instance->strong_count = immortal ? -1 : 1;
    instance->weak_count = 0;

    return instance;
}

static OBJECT_PTR RcNewArray(struct DynArrayClass* array_class, int count, bool immortal) {
    OBJECT_PTR instance = RcNew(&array_class->base, immortal);

    array_class->alloc(instance, count, NULL, NULL);

    return instance;
}

static void RcRetain(OBJECT_PTR object, bool weak) {
    if (!object) {
        return;
    }
    
    // don't retain immortal refs
    if (object->strong_count < 0) {
        return;
    }
    
    if (weak) {
        object->weak_count += 1;
    } else {
        if (object->strong_count == 0) {
            fatal("resurrecting with 0 strong refs pointer @ 0x%p (+ %d weak refs remain)", object, object->weak_count);
        }
    
        object->strong_count += 1;
    }

#if TRACE_RC
    // safe to use the name chars ptr as a C string here, we null-terminate string literals
    printf("rc: retain %s @ 0x%p (%d+%d refs)\n", 
        OBJECT_DISPLAY(object), 
        object, 
        object->strong_count, 
        object->weak_count);
#endif
}

static bool RcRelease(OBJECT_PTR object, bool weak) {
    if (!object) {
        // releasing NULL should be ignored
        return false;
    }

    if (object->strong_count < 0) {
        // immortal
        return false;
    }
   
    if (weak) {
        if (object->weak_count == 0) {
            fatal("releasing with 0 weak refs remaining @ 0x%p", object);
        }

#if TRACE_RC
        printf("rc: release %s @ 0x%p (%d+%d remain)\n", OBJECT_DISPLAY(object), object, object->strong_count, object->weak_count - 1);
#endif
        
        object->weak_count -= 1;
    } else {
        if (object->strong_count == 0) {
            fatal("releasing with 0 strong refs remaining @ 0x%p", object);
        }

#if TRACE_RC
        printf("rc: release %s @ 0x%p (%d+%d remain)\n", OBJECT_DISPLAY(object), object, object->strong_count - 1, object->weak_count);
#endif

        // call the dtor before decrementing the ref count, because it must still be a live reference
        // while the function is executing
        if (object->strong_count == 1 && object->class->dtor) {
#if TRACE_RC
            printf("rc: \tdisposing %s @ 0x%p\n", OBJECT_DISPLAY(object), object);
#endif
            object->class->dtor(object);
            
            // invoke structural release to release struct fields
            if (object->class->cleanup) {
                object->class->cleanup(object);
            }
            object->class = NULL;

            if (object->strong_count != 1) {
                fprintf(stderr, "destructor for %s modified the reference count of the destroyed instance\n", OBJECT_DISPLAY(object));
                fflush(stderr);
                abort();
            }
        }
  
        object->strong_count -= 1;
    }

    if (object->strong_count == 0 && object->weak_count == 0) {
        // free memory
        Free(object);
        return true;
    }
    
    return false;
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
    STRING_STRUCT* str = (STRING_STRUCT*) RcNew(&STRING_CLASS, false);
    STRING_LEN(str) = (int32_t) len;
    STRING_CHARS(str) = System_GetMem(len);
    memcpy(STRING_CHARS(str), buf, len);

    return str;
}

static int32_t System_ArrayLengthInternal(OBJECT_PTR arr) {
    if (!arr || arr->strong_count == 0) {
        fatal("called Length for an invalid array pointer");
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr->class;

    return array_class->length(arr);
}

static OBJECT_PTR System_ArraySetLengthInternal(
    OBJECT_PTR arr,
    int32_t new_len,
    void* default_val
) {
    if (!arr || arr->strong_count <= 0) {
        // note that it's illegal to resize an immortal array
        fatal("called SetLength for an invalid array pointer");
    }

    struct DynArrayClass* array_class = (struct DynArrayClass*) arr->class;

    OBJECT_PTR new_arr = RcNewArray(array_class, new_len, false);
    array_class->alloc(new_arr, new_len, (OBJECT_PTR) arr, default_val);

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

static double System_Time(void) {
    struct timespec current_time = {};
    if (timespec_get(&current_time, TIME_UTC) != TIME_UTC) {
        return 0.0;
    }

    return (double)current_time.tv_sec + (current_time.tv_nsec / 1000000000.0);
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

#ifndef DISABLE_RTTI

static void InvokeMethod(METHODINFO_STRUCT* method, void* instance, void** args, int32_t arg_count, void* out_result) {
    Invoker invoker = METHODINFO_INVOKER(method);
    if (!invoker) {
        fatal("InvokeMethod called for abstract method");
    }

    if (instance) {
        void** all_args = (void**) STACKALLOC(sizeof(void*) * (arg_count + 1));
        all_args[0] = instance;

        memcpy(all_args + 1, args, arg_count * sizeof(void*));

        invoker(all_args, out_result);
    } else {
        invoker(args, out_result);
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

static TYPEINFO_STRUCT* System_GetObjectTypeInfo(OBJECT_PTR object) {
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

static void InvokeFunction(FUNCINFO_STRUCT* func, void** args, int32_t arg_count, void* out_result) {
    Invoker invoker = FUNCINFO_INVOKER(func);
    if (!invoker) {
        fatal("InvokeFunction called for non-invokable func");
    }

    invoker(args, out_result);
}

#endif // DISABLE_RTTI
