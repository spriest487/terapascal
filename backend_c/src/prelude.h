#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>

#ifdef _MSC_VER
#include <malloc.h>
#endif

// forward decl of builtin types
STRING_STRUCT;
TYPEINFO_STRUCT;
METHODINFO_STRUCT;
FUNCINFO_STRUCT;

typedef void (*DestructorFunc)(void*);

typedef void (*RcCleanupFunc)(void*);
typedef void (*RcRetainFunc)(void*);

#ifdef _MSC_VER
#   define PACKED_DECL(DECL) __pragma(pack(push, 1)) DECL __pragma(pack(pop))
#else
#   define PACKED_DECL(DECL) DECL __attribute__((__packed__))
#endif

// classes and interfaces runtime support

struct MethodTable {
    size_t iface;
    struct MethodTable* next;
};

struct Class {
    size_t size;
    
    TYPEINFO_STRUCT* typeinfo;

    struct MethodTable* iface_methods;

    RcCleanupFunc cleanup;
    DestructorFunc dtor;
};

struct Rc {
    struct Class* class;
    int32_t strong_count;
    int32_t weak_count;
};

#define MAKE_RC(className, strong, weak) { \
    .class = (struct Class*) &className,\
    .strong_count = strong,\
    .weak_count = weak\
}

#define OBJECT_PTR struct Rc*

typedef void (*Invoker)(void** args, void* resultOut);

typedef void (*DynArrayAlloc)(OBJECT_PTR arr, int32_t len, OBJECT_PTR copy_from, void* default_val);
typedef int32_t (*DynArrayLength)(OBJECT_PTR arr);
typedef void* (*DynArrayGetElement)(OBJECT_PTR arr, int32_t index);

struct DynArrayClass {
    struct Class base;

    DynArrayAlloc alloc;
    DynArrayLength length;
    DynArrayGetElement get_element;
};

static void DynArrayBoundsCheck(OBJECT_PTR arr, int32_t index); 

static bool IsImpl(struct Class* class, size_t iface) {
    struct MethodTable* next = class->iface_methods;
    while (next) {
        if (next->iface == iface) {
            return true;
        }
        next = next->next;
    }

    return false;
}

// closure types aren't known by their callers, but all follow this layout so
// we can cast pointers to this to access the function pointer
struct AnonymousClosure {
    struct Rc rc;
    void (*func_ptr)(void*);
};

// internal memory allocation

#ifdef TRACE_HEAP

    struct AllocTrace {
        void* at;
        size_t len;
        struct AllocTrace* next;
    };

    static struct AllocTrace* alloc_traces;

#endif

static void* Alloc(size_t len);
static void Free(void* mem);

// RC runtime functions

static void RcRetain(OBJECT_PTR instance, bool weak);
static bool RcRelease(OBJECT_PTR instance, bool weak);
static OBJECT_PTR RcNew(struct Class* class, bool immortal);
static OBJECT_PTR RcNewArray(struct DynArrayClass* class, int count, bool immortal);

_Noreturn static void Raise(STRING_STRUCT* msg_str);

static void InvokeMethod(METHODINFO_STRUCT* method, void* instance, void** args, int32_t arg_count, void* out_result);
static void InvokeFunction(FUNCINFO_STRUCT* func, void** args, int32_t arg_count, void* out_result);

static TYPEINFO_STRUCT** typeinfo_list;
static int32_t typeinfo_count;

static TYPEINFO_STRUCT* System_FindTypeInfo(STRING_STRUCT* type_name);
static int System_GetTypeInfoCount(void);
static TYPEINFO_STRUCT* System_GetTypeInfoByIndex(int type_index);
static TYPEINFO_STRUCT* System_GetObjectTypeInfo(OBJECT_PTR obj);

static FUNCINFO_STRUCT** funcinfo_list;
static int32_t funcinfo_count;

static FUNCINFO_STRUCT* System_FindFunctionInfo(STRING_STRUCT* func_name);
static int System_GetFunctionInfoCount(void);
static FUNCINFO_STRUCT* System_GetFunctionInfoByIndex(int func_index);

// implementations of System unit builtins

static int32_t System_StrToInt(STRING_STRUCT* str);

static STRING_STRUCT* System_Int8ToStr(int8_t i);
static STRING_STRUCT* System_ByteToStr(uint8_t i);
static STRING_STRUCT* System_Int16ToStr(int16_t i);
static STRING_STRUCT* System_UInt16ToStr(uint16_t i);
static STRING_STRUCT* System_IntToStr(int32_t i);
static STRING_STRUCT* System_UInt32ToStr(uint32_t i);
static STRING_STRUCT* System_Int64ToStr(int64_t i);
static STRING_STRUCT* System_UInt64ToStr(uint64_t i);
static STRING_STRUCT* System_NativeIntToStr(ptrdiff_t i);
static STRING_STRUCT* System_NativeUIntToStr(size_t i);
static STRING_STRUCT* System_PointerToStr(const void* i);
static STRING_STRUCT* System_RealToStr(float f);

static unsigned char* System_GetMem(int32_t len);
static void System_FreeMem(unsigned char* mem);
static void System_Write(STRING_STRUCT* str);
static void System_WriteLn(STRING_STRUCT* str);
static STRING_STRUCT* System_ReadLn(void);
static int32_t System_ArrayLengthInternal(OBJECT_PTR arr);
static OBJECT_PTR System_ArraySetLengthInternal(OBJECT_PTR arr, int32_t new_len, void* default_val);

static int32_t System_RandomInteger(int32_t from, int32_t to);
static float System_RandomSingle(float from, float to);

static double System_Time(void);

static float System_Pow(float val, float pow);
static float System_Sqrt(float val);
static float System_Sin(float val);
static float System_ArcSin(float val);
static float System_Cos(float val);
static float System_ArcCos(float val);
static float System_Tan(float val);
static float System_ArcTan(float val);

static float System_Infinity(void);
static bool System_IsInfinite(float val);
static float System_NaN(void);
static bool System_IsNaN(float val);

// runtime start/stop

// this needs to match what would ordinarily be generated for the System.String decl
PACKED_DECL(STRING_STRUCT {
    struct Rc rc;

    unsigned char* field_0;
    int32_t field_1;
});

#define MAKE_STRING_LIT(text) {\
    .rc = MAKE_RC(STRING_CLASS, -1, 0),\
    .field_0 = (unsigned char*) text,\
    .field_1 = sizeof(text) / sizeof(text[0]) - 1,\
}

static void ModuleInit(void);

static void* LoadSymbol(const char* src, const char* sym);

static void RuntimeExit(int code) {
#if TRACE_HEAP
    struct AllocTrace* leaked_alloc = alloc_traces;
    while (leaked_alloc) {
        fprintf(stderr, "heap: leaked allocation of length %4zu at 0x%p\n", leaked_alloc->len, leaked_alloc->at);
        leaked_alloc = leaked_alloc->next;
    }
#endif

    exit(code);
}

int main() {
    ModuleInit();
    RuntimeExit(0);
}
