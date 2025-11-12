#if defined(__clang__) || defined(__GNUC__)
   #define CONV_CDECL __attribute__ ((cdecl))
#else
    #define CONV_CDECL __cdecl
#endif

#ifdef _WIN32
    #define EXPORT_FN __declspec(dllexport)
#else
    #define EXPORT_FN
#endif

#include <stdio.h>
#include <stdint.h>

EXPORT_FN void CONV_CDECL X(void) {
    printf("native call to X()\n");
    fflush(stdout);
}

EXPORT_FN void CONV_CDECL Y(int32_t val) {
    printf("native call to Y(%d)\n", val);
    fflush(stdout);
}

EXPORT_FN int32_t CONV_CDECL Z(void) {
    printf("native call to Z\n");
    fflush(stdout);
    return 123456;
}

static int int_value = 7654321;

EXPORT_FN int* CONV_CDECL ReturnsIntPtr(void) {
    printf("native call to ReturnsIntPtr\n");
    return &int_value;
}

EXPORT_FN void CONV_CDECL PrintIntPtr(int* int_ptr) {
    printf("native call to PrintIntPtr(%d)\n", *int_ptr);
    fflush(stdout);
}
