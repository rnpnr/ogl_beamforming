/* See LICENSE for license details. */
// NOTE(rnp): an almost single file build for glslang

#include "../../compiler.h"

#include "SPIRV/SpvBuilder.cpp"

// NOTE(rnp): cannot be included at the samee time as glslang_c_interface.cpp
// compiled as a seperate object
//#include "SPIRV/CInterface/spirv_c_interface.cpp"

#include "SPIRV/GlslangToSpv.cpp"
#include "SPIRV/InReadableOrder.cpp"
#include "SPIRV/Logger.cpp"
#include "SPIRV/SpvPostProcess.cpp"
#include "SPIRV/SpvTools.cpp"
#include "SPIRV/disassemble.cpp"
#include "SPIRV/doc.cpp"
#include "glslang/CInterface/glslang_c_interface.cpp"
#include "glslang/GenericCodeGen/CodeGen.cpp"
#include "glslang/GenericCodeGen/Link.cpp"
#include "glslang/MachineIndependent/Constant.cpp"
#include "glslang/MachineIndependent/InfoSink.cpp"
#include "glslang/MachineIndependent/Initialize.cpp"
#include "glslang/MachineIndependent/IntermTraverse.cpp"
#include "glslang/MachineIndependent/Intermediate.cpp"
#include "glslang/MachineIndependent/ParseContextBase.cpp"
#include "glslang/MachineIndependent/ParseHelper.cpp"
#include "glslang/MachineIndependent/PoolAlloc.cpp"
#include "glslang/MachineIndependent/RemoveTree.cpp"
#include "glslang/MachineIndependent/Scan.cpp"
#include "glslang/MachineIndependent/ShaderLang.cpp"
#include "glslang/MachineIndependent/SpirvIntrinsics.cpp"
#include "glslang/MachineIndependent/SymbolTable.cpp"
#include "glslang/MachineIndependent/Versions.cpp"
#include "glslang/MachineIndependent/attribute.cpp"
#include "glslang/MachineIndependent/intermOut.cpp"
#include "glslang/MachineIndependent/iomapper.cpp"
#include "glslang/MachineIndependent/limits.cpp"
#include "glslang/MachineIndependent/linkValidate.cpp"
#include "glslang/MachineIndependent/parseConst.cpp"
#include "glslang/MachineIndependent/preprocessor/Pp.cpp"
#include "glslang/MachineIndependent/preprocessor/PpAtom.cpp"
#include "glslang/MachineIndependent/preprocessor/PpContext.cpp"
#include "glslang/MachineIndependent/preprocessor/PpScanner.cpp"
#include "glslang/MachineIndependent/preprocessor/PpTokens.cpp"
#include "glslang/MachineIndependent/propagateNoContraction.cpp"
#include "glslang/MachineIndependent/reflection.cpp"
#include "glslang/ResourceLimits/ResourceLimits.cpp"
#include "glslang/ResourceLimits/resource_limits_c.cpp"

#if OS_WINDOWS
// NOTE(rnp): includes windows.h: i.e. it needs its own TU
//#include "glslang/OSDependent/Windows/ossource.cpp"
#else
#include "glslang/OSDependent/Unix/ossource.cpp"
#endif

// NOTE(rnp): generated with some of the worst garbage ever taught in CS (yacc). must come last
#include "glslang/MachineIndependent/glslang_tab.cpp"
