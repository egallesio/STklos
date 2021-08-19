/* This file was generated automatically. DO NOT EDIT */

#ifndef _VM_H
#define _VM_H
#  define NOP                  0
#  define IM_FALSE             1
#  define IM_TRUE              2
#  define IM_NIL               3
#  define IM_MINUS1            4
#  define IM_ZERO              5
#  define IM_ONE               6
#  define IM_VOID              7
#  define SMALL_INT            8
#  define CONSTANT             9
#  define GLOBAL_REF           10
#  define UGLOBAL_REF          11
#  define LOCAL_REF0           12
#  define LOCAL_REF1           13
#  define LOCAL_REF2           14
#  define LOCAL_REF3           15
#  define LOCAL_REF4           16
#  define LOCAL_REF            17
#  define DEEP_LOCAL_REF       18
#  define GLOBAL_SET           19
#  define UGLOBAL_SET          20
#  define LOCAL_SET0           21
#  define LOCAL_SET1           22
#  define LOCAL_SET2           23
#  define LOCAL_SET3           24
#  define LOCAL_SET4           25
#  define LOCAL_SET            26
#  define DEEP_LOCAL_SET       27
#  define GOTO                 28
#  define JUMP_FALSE           29
#  define JUMP_TRUE            30
#  define DEFINE_SYMBOL        31
#  define POP                  32
#  define PUSH                 33
#  define DBG_VM               34
#  define CREATE_CLOSURE       35
#  define RETURN               36
#  define PREPARE_CALL         37
#  define INVOKE               38
#  define TAIL_INVOKE          39
#  define ENTER_LET_STAR       40
#  define ENTER_LET            41
#  define ENTER_TAIL_LET_STAR  42
#  define ENTER_TAIL_LET       43
#  define LEAVE_LET            44
#  define PUSH_HANDLER         45
#  define POP_HANDLER          46
#  define END_OF_CODE          47
#  define IN_ADD2              48
#  define IN_SUB2              49
#  define IN_MUL2              50
#  define IN_DIV2              51
#  define IN_NUMEQ             52
#  define IN_NUMLT             53
#  define IN_NUMGT             54
#  define IN_NUMLE             55
#  define IN_NUMGE             56
#  define IN_INCR              57
#  define IN_DECR              58
#  define IN_CONS              59
#  define IN_NULLP             60
#  define IN_CAR               61
#  define IN_CDR               62
#  define IN_LIST              63
#  define IN_NOT               64
#  define IN_VREF              65
#  define IN_VSET              66
#  define IN_SREF              67
#  define IN_SSET              68
#  define IN_EQ                69
#  define IN_EQV               70
#  define IN_EQUAL             71
#  define IN_APPLY             72
#  define MAKE_EXPANDER        73
#  define SET_CUR_MOD          74
#  define DOCSTRG              75
#  define FORMALS              76
#  define FALSE_PUSH           77
#  define TRUE_PUSH            78
#  define NIL_PUSH             79
#  define MINUS1_PUSH          80
#  define ZERO_PUSH            81
#  define ONE_PUSH             82
#  define VOID_PUSH            83
#  define INT_PUSH             84
#  define CONSTANT_PUSH        85
#  define GREF_INVOKE          86
#  define UGREF_INVOKE         87
#  define IN_NUMDIFF           88
#  define IN_NOT_EQ            89
#  define IN_NOT_EQV           90
#  define IN_NOT_EQUAL         91
#  define JUMP_NUMDIFF         92
#  define JUMP_NUMEQ           93
#  define JUMP_NUMLT           94
#  define JUMP_NUMLE           95
#  define JUMP_NUMGT           96
#  define JUMP_NUMGE           97
#  define JUMP_NOT_EQ          98
#  define JUMP_NOT_EQV         99
#  define JUMP_NOT_EQUAL       100
#  define LOCAL_REF0_PUSH      101
#  define LOCAL_REF1_PUSH      102
#  define LOCAL_REF2_PUSH      103
#  define LOCAL_REF3_PUSH      104
#  define LOCAL_REF4_PUSH      105
#  define GLOBAL_REF_PUSH      106
#  define UGLOBAL_REF_PUSH     107
#  define GREF_TAIL_INVOKE     108
#  define UGREF_TAIL_INVOKE    109
#  define PUSH_PREPARE_CALL    110
#  define PUSH_GLOBAL_REF      111
#  define PUSH_UGLOBAL_REF     112
#  define PUSH_GREF_INVOKE     113
#  define PUSH_UGREF_INVOKE    114
#  define PUSH_GREF_TAIL_INV   115
#  define PUSH_UGREF_TAIL_INV  116
#  define DEEP_LOC_REF_PUSH    117
#  define UNUSED_3             118
#  define UNUSED_4             119
#  define UNUSED_5             120
#  define UNUSED_6             121
#  define UNUSED_7             122
#  define UNUSED_8             123
#  define UNUSED_9             124
#  define UNUSED_10            125
#  define UNUSED_11            126
#  define UNUSED_12            127
#  define UNUSED_13            128
#  define UNUSED_14            129
#  define UNUSED_15            130
#  define UNUSED_16            131
#  define UNUSED_17            132
#  define UNUSED_18            133
#  define UNUSED_19            134
#  define IN_SINT_ADD2         135
#  define IN_SINT_SUB2         136
#  define IN_SINT_MUL2         137
#  define IN_SINT_DIV2         138
#  define UNUSED_20            139
#  define UNUSED_21            140
#  define UNUSED_22            141
#  define UNUSED_23            142
#  define UNUSED_24            143
#  define UNUSED_25            144
#  define UNUSED_26            145
#  define UNUSED_27            146
#  define UNUSED_28            147
#  define CALL_LOCATION        148
#  define DEEP_LOC_REF_FAR     149
#  define DEEP_LOC_SET_FAR     150
#  define CREATE_CLOSURE_FAR   151
#  define PUSH_HANDLER_FAR     152
#  define IN_FXADD2            153
#  define IN_FXSUB2            154
#  define IN_FXMUL2            155
#  define IN_FXDIV2            156
#  define IN_SINT_FXADD2       157
#  define IN_SINT_FXSUB2       158
#  define IN_SINT_FXMUL2       159
#  define IN_SINT_FXDIV2       160
#  define IN_FXEQ              161
#  define IN_FXLT              162
#  define IN_FXGT              163
#  define IN_FXLE              164
#  define IN_FXGE              165

#  define NB_VM_INSTR (IN_FXGE             +1)
#endif



#ifdef DEFINE_JUMP_TABLE
static void *jump_table[] = {
  &&lab_NOP                 ,
  &&lab_IM_FALSE            ,
  &&lab_IM_TRUE             ,
  &&lab_IM_NIL              ,
  &&lab_IM_MINUS1           ,
  &&lab_IM_ZERO             ,
  &&lab_IM_ONE              ,
  &&lab_IM_VOID             ,
  &&lab_SMALL_INT           ,
  &&lab_CONSTANT            ,
  &&lab_GLOBAL_REF          ,
  &&lab_UGLOBAL_REF         ,
  &&lab_LOCAL_REF0          ,
  &&lab_LOCAL_REF1          ,
  &&lab_LOCAL_REF2          ,
  &&lab_LOCAL_REF3          ,
  &&lab_LOCAL_REF4          ,
  &&lab_LOCAL_REF           ,
  &&lab_DEEP_LOCAL_REF      ,
  &&lab_GLOBAL_SET          ,
  &&lab_UGLOBAL_SET         ,
  &&lab_LOCAL_SET0          ,
  &&lab_LOCAL_SET1          ,
  &&lab_LOCAL_SET2          ,
  &&lab_LOCAL_SET3          ,
  &&lab_LOCAL_SET4          ,
  &&lab_LOCAL_SET           ,
  &&lab_DEEP_LOCAL_SET      ,
  &&lab_GOTO                ,
  &&lab_JUMP_FALSE          ,
  &&lab_JUMP_TRUE           ,
  &&lab_DEFINE_SYMBOL       ,
  &&lab_POP                 ,
  &&lab_PUSH                ,
  &&lab_DBG_VM              ,
  &&lab_CREATE_CLOSURE      ,
  &&lab_RETURN              ,
  &&lab_PREPARE_CALL        ,
  &&lab_INVOKE              ,
  &&lab_TAIL_INVOKE         ,
  &&lab_ENTER_LET_STAR      ,
  &&lab_ENTER_LET           ,
  &&lab_ENTER_TAIL_LET_STAR ,
  &&lab_ENTER_TAIL_LET      ,
  &&lab_LEAVE_LET           ,
  &&lab_PUSH_HANDLER        ,
  &&lab_POP_HANDLER         ,
  &&lab_END_OF_CODE         ,
  &&lab_IN_ADD2             ,
  &&lab_IN_SUB2             ,
  &&lab_IN_MUL2             ,
  &&lab_IN_DIV2             ,
  &&lab_IN_NUMEQ            ,
  &&lab_IN_NUMLT            ,
  &&lab_IN_NUMGT            ,
  &&lab_IN_NUMLE            ,
  &&lab_IN_NUMGE            ,
  &&lab_IN_INCR             ,
  &&lab_IN_DECR             ,
  &&lab_IN_CONS             ,
  &&lab_IN_NULLP            ,
  &&lab_IN_CAR              ,
  &&lab_IN_CDR              ,
  &&lab_IN_LIST             ,
  &&lab_IN_NOT              ,
  &&lab_IN_VREF             ,
  &&lab_IN_VSET             ,
  &&lab_IN_SREF             ,
  &&lab_IN_SSET             ,
  &&lab_IN_EQ               ,
  &&lab_IN_EQV              ,
  &&lab_IN_EQUAL            ,
  &&lab_IN_APPLY            ,
  &&lab_MAKE_EXPANDER       ,
  &&lab_SET_CUR_MOD         ,
  &&lab_DOCSTRG             ,
  &&lab_FORMALS             ,
  &&lab_FALSE_PUSH          ,
  &&lab_TRUE_PUSH           ,
  &&lab_NIL_PUSH            ,
  &&lab_MINUS1_PUSH         ,
  &&lab_ZERO_PUSH           ,
  &&lab_ONE_PUSH            ,
  &&lab_VOID_PUSH           ,
  &&lab_INT_PUSH            ,
  &&lab_CONSTANT_PUSH       ,
  &&lab_GREF_INVOKE         ,
  &&lab_UGREF_INVOKE        ,
  &&lab_IN_NUMDIFF          ,
  &&lab_IN_NOT_EQ           ,
  &&lab_IN_NOT_EQV          ,
  &&lab_IN_NOT_EQUAL        ,
  &&lab_JUMP_NUMDIFF        ,
  &&lab_JUMP_NUMEQ          ,
  &&lab_JUMP_NUMLT          ,
  &&lab_JUMP_NUMLE          ,
  &&lab_JUMP_NUMGT          ,
  &&lab_JUMP_NUMGE          ,
  &&lab_JUMP_NOT_EQ         ,
  &&lab_JUMP_NOT_EQV        ,
  &&lab_JUMP_NOT_EQUAL      ,
  &&lab_LOCAL_REF0_PUSH     ,
  &&lab_LOCAL_REF1_PUSH     ,
  &&lab_LOCAL_REF2_PUSH     ,
  &&lab_LOCAL_REF3_PUSH     ,
  &&lab_LOCAL_REF4_PUSH     ,
  &&lab_GLOBAL_REF_PUSH     ,
  &&lab_UGLOBAL_REF_PUSH    ,
  &&lab_GREF_TAIL_INVOKE    ,
  &&lab_UGREF_TAIL_INVOKE   ,
  &&lab_PUSH_PREPARE_CALL   ,
  &&lab_PUSH_GLOBAL_REF     ,
  &&lab_PUSH_UGLOBAL_REF    ,
  &&lab_PUSH_GREF_INVOKE    ,
  &&lab_PUSH_UGREF_INVOKE   ,
  &&lab_PUSH_GREF_TAIL_INV  ,
  &&lab_PUSH_UGREF_TAIL_INV ,
  &&lab_DEEP_LOC_REF_PUSH   ,
  &&lab_UNUSED_3            ,
  &&lab_UNUSED_4            ,
  &&lab_UNUSED_5            ,
  &&lab_UNUSED_6            ,
  &&lab_UNUSED_7            ,
  &&lab_UNUSED_8            ,
  &&lab_UNUSED_9            ,
  &&lab_UNUSED_10           ,
  &&lab_UNUSED_11           ,
  &&lab_UNUSED_12           ,
  &&lab_UNUSED_13           ,
  &&lab_UNUSED_14           ,
  &&lab_UNUSED_15           ,
  &&lab_UNUSED_16           ,
  &&lab_UNUSED_17           ,
  &&lab_UNUSED_18           ,
  &&lab_UNUSED_19           ,
  &&lab_IN_SINT_ADD2        ,
  &&lab_IN_SINT_SUB2        ,
  &&lab_IN_SINT_MUL2        ,
  &&lab_IN_SINT_DIV2        ,
  &&lab_UNUSED_20           ,
  &&lab_UNUSED_21           ,
  &&lab_UNUSED_22           ,
  &&lab_UNUSED_23           ,
  &&lab_UNUSED_24           ,
  &&lab_UNUSED_25           ,
  &&lab_UNUSED_26           ,
  &&lab_UNUSED_27           ,
  &&lab_UNUSED_28           ,
  &&lab_CALL_LOCATION       ,
  &&lab_DEEP_LOC_REF_FAR    ,
  &&lab_DEEP_LOC_SET_FAR    ,
  &&lab_CREATE_CLOSURE_FAR  ,
  &&lab_PUSH_HANDLER_FAR    ,
  &&lab_IN_FXADD2           ,
  &&lab_IN_FXSUB2           ,
  &&lab_IN_FXMUL2           ,
  &&lab_IN_FXDIV2           ,
  &&lab_IN_SINT_FXADD2      ,
  &&lab_IN_SINT_FXSUB2      ,
  &&lab_IN_SINT_FXMUL2      ,
  &&lab_IN_SINT_FXDIV2      ,
  &&lab_IN_FXEQ             ,
  &&lab_IN_FXLT             ,
  &&lab_IN_FXGT             ,
  &&lab_IN_FXLE             ,
  &&lab_IN_FXGE             ,
  NULL};
#endif
#undef DEFINE_JUMP_TABLE



#ifdef DEFINE_NAME_TABLE
static char *name_table[] = {
  "NOP                 ",
  "IM_FALSE            ",
  "IM_TRUE             ",
  "IM_NIL              ",
  "IM_MINUS1           ",
  "IM_ZERO             ",
  "IM_ONE              ",
  "IM_VOID             ",
  "SMALL_INT           ",
  "CONSTANT            ",
  "GLOBAL_REF          ",
  "UGLOBAL_REF         ",
  "LOCAL_REF0          ",
  "LOCAL_REF1          ",
  "LOCAL_REF2          ",
  "LOCAL_REF3          ",
  "LOCAL_REF4          ",
  "LOCAL_REF           ",
  "DEEP_LOCAL_REF      ",
  "GLOBAL_SET          ",
  "UGLOBAL_SET         ",
  "LOCAL_SET0          ",
  "LOCAL_SET1          ",
  "LOCAL_SET2          ",
  "LOCAL_SET3          ",
  "LOCAL_SET4          ",
  "LOCAL_SET           ",
  "DEEP_LOCAL_SET      ",
  "GOTO                ",
  "JUMP_FALSE          ",
  "JUMP_TRUE           ",
  "DEFINE_SYMBOL       ",
  "POP                 ",
  "PUSH                ",
  "DBG_VM              ",
  "CREATE_CLOSURE      ",
  "RETURN              ",
  "PREPARE_CALL        ",
  "INVOKE              ",
  "TAIL_INVOKE         ",
  "ENTER_LET_STAR      ",
  "ENTER_LET           ",
  "ENTER_TAIL_LET_STAR ",
  "ENTER_TAIL_LET      ",
  "LEAVE_LET           ",
  "PUSH_HANDLER        ",
  "POP_HANDLER         ",
  "END_OF_CODE         ",
  "IN_ADD2             ",
  "IN_SUB2             ",
  "IN_MUL2             ",
  "IN_DIV2             ",
  "IN_NUMEQ            ",
  "IN_NUMLT            ",
  "IN_NUMGT            ",
  "IN_NUMLE            ",
  "IN_NUMGE            ",
  "IN_INCR             ",
  "IN_DECR             ",
  "IN_CONS             ",
  "IN_NULLP            ",
  "IN_CAR              ",
  "IN_CDR              ",
  "IN_LIST             ",
  "IN_NOT              ",
  "IN_VREF             ",
  "IN_VSET             ",
  "IN_SREF             ",
  "IN_SSET             ",
  "IN_EQ               ",
  "IN_EQV              ",
  "IN_EQUAL            ",
  "IN_APPLY            ",
  "MAKE_EXPANDER       ",
  "SET_CUR_MOD         ",
  "DOCSTRG             ",
  "FORMALS             ",
  "FALSE_PUSH          ",
  "TRUE_PUSH           ",
  "NIL_PUSH            ",
  "MINUS1_PUSH         ",
  "ZERO_PUSH           ",
  "ONE_PUSH            ",
  "VOID_PUSH           ",
  "INT_PUSH            ",
  "CONSTANT_PUSH       ",
  "GREF_INVOKE         ",
  "UGREF_INVOKE        ",
  "IN_NUMDIFF          ",
  "IN_NOT_EQ           ",
  "IN_NOT_EQV          ",
  "IN_NOT_EQUAL        ",
  "JUMP_NUMDIFF        ",
  "JUMP_NUMEQ          ",
  "JUMP_NUMLT          ",
  "JUMP_NUMLE          ",
  "JUMP_NUMGT          ",
  "JUMP_NUMGE          ",
  "JUMP_NOT_EQ         ",
  "JUMP_NOT_EQV        ",
  "JUMP_NOT_EQUAL      ",
  "LOCAL_REF0_PUSH     ",
  "LOCAL_REF1_PUSH     ",
  "LOCAL_REF2_PUSH     ",
  "LOCAL_REF3_PUSH     ",
  "LOCAL_REF4_PUSH     ",
  "GLOBAL_REF_PUSH     ",
  "UGLOBAL_REF_PUSH    ",
  "GREF_TAIL_INVOKE    ",
  "UGREF_TAIL_INVOKE   ",
  "PUSH_PREPARE_CALL   ",
  "PUSH_GLOBAL_REF     ",
  "PUSH_UGLOBAL_REF    ",
  "PUSH_GREF_INVOKE    ",
  "PUSH_UGREF_INVOKE   ",
  "PUSH_GREF_TAIL_INV  ",
  "PUSH_UGREF_TAIL_INV ",
  "DEEP_LOC_REF_PUSH   ",
  "UNUSED_3            ",
  "UNUSED_4            ",
  "UNUSED_5            ",
  "UNUSED_6            ",
  "UNUSED_7            ",
  "UNUSED_8            ",
  "UNUSED_9            ",
  "UNUSED_10           ",
  "UNUSED_11           ",
  "UNUSED_12           ",
  "UNUSED_13           ",
  "UNUSED_14           ",
  "UNUSED_15           ",
  "UNUSED_16           ",
  "UNUSED_17           ",
  "UNUSED_18           ",
  "UNUSED_19           ",
  "IN_SINT_ADD2        ",
  "IN_SINT_SUB2        ",
  "IN_SINT_MUL2        ",
  "IN_SINT_DIV2        ",
  "UNUSED_20           ",
  "UNUSED_21           ",
  "UNUSED_22           ",
  "UNUSED_23           ",
  "UNUSED_24           ",
  "UNUSED_25           ",
  "UNUSED_26           ",
  "UNUSED_27           ",
  "UNUSED_28           ",
  "CALL_LOCATION       ",
  "DEEP_LOC_REF_FAR    ",
  "DEEP_LOC_SET_FAR    ",
  "CREATE_CLOSURE_FAR  ",
  "PUSH_HANDLER_FAR    ",
  "IN_FXADD2           ",
  "IN_FXSUB2           ",
  "IN_FXMUL2           ",
  "IN_FXDIV2           ",
  "IN_SINT_FXADD2      ",
  "IN_SINT_FXSUB2      ",
  "IN_SINT_FXMUL2      ",
  "IN_SINT_FXDIV2      ",
  "IN_FXEQ             ",
  "IN_FXLT             ",
  "IN_FXGT             ",
  "IN_FXLE             ",
  "IN_FXGE             ",
  NULL};
#endif
#undef DEFINE_NAME_TABLE
