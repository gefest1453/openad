#include "ti_si.h"
SI_RESOURCE resource9 = {"issue",0,6,0,0};
SI_RESOURCE resource10 = {"sem",1,1,0,4};
SI_RESOURCE resource11 = {"integer_or_memory",2,4,0,6};
SI_RESOURCE resource12 = {"memory",3,2,0,10};
SI_RESOURCE resource13 = {"memory0",4,1,0,13};
SI_RESOURCE resource14 = {"floating-point",5,2,0,15};
SI_RESOURCE resource15 = {"floating-point0",6,1,0,18};
SI_RESOURCE resource16 = {"integer",7,2,0,20};
SI_RESOURCE resource17 = {"integer0",8,1,0,23};
SI_RESOURCE resource18 = {"branch",9,3,0,25};
SI_RESOURCE resource19 = {"B0_or_B1",10,2,0,28};
SI_RESOURCE resource20 = {"B0_or_B2",11,2,0,31};
SI_RESOURCE resource21 = {"B2",12,1,0,34};
const int SI_resource_count = 13;
SI_RESOURCE * const SI_resources[] = {
  &resource9,
  &resource10,
  &resource11,
  &resource12,
  &resource13,
  &resource14,
  &resource15,
  &resource16,
  &resource17,
  &resource18,
  &resource19,
  &resource20,
  &resource21
};
const SI_RRW SI_RRW_initializer = 0x901084c1;
const SI_RRW SI_RRW_overuse_mask = 0xa494a5228;
const int SI_issue_slot_count = 0;
SI_ISSUE_SLOT * const SI_issue_slots[1] = {0};

/* Instruction group Dummy instructions */
static const SI_RRW res_req1[] = {
  0
};
static const mUINT8 latency5[] = {0,0,0,0,0,0};
static const mUINT8 latency6[] = {0,0,0};
static SI gname0 = {
  "Dummy instructions",
  0              , /* id */
  latency5       , /* operand latency */
  latency6       , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req1       , /* resource requirement */
  0              , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  0              , /* resource count vec size */
  0              , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group BR */
static const SI_RRW res_req23[] = {
  1,
  0x2000001
};
static const SI_RESOURCE_ID_SET gname25[] = {
  0x201
};
static SI_RESOURCE_TOTAL gname24[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */
};
static const mUINT8 latency27[] = {0,0,0,0,0,0};
static const mUINT8 latency28[] = {1,1,1};
static SI gname22 = {
  "BR",
  1              , /* id */
  latency27      , /* operand latency */
  latency28      , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req23      , /* resource requirement */
  gname25        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname24        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group BR_B2 */
static const SI_RRW res_req32[] = {
  1,
  0x402000001
};
static const SI_RESOURCE_ID_SET gname34[] = {
  0x1201
};
static SI_RESOURCE_TOTAL gname33[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */,
  {&resource21,1} /* B2 */
};
static const mUINT8 latency36[] = {0,0,0,0,0,0};
static const mUINT8 latency37[] = {1,1,1};
static SI gname31 = {
  "BR_B2",
  2              , /* id */
  latency36      , /* operand latency */
  latency37      , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req32      , /* resource requirement */
  gname34        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname33        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group BRP */
static const SI_RRW res_req41[] = {
  1,
  0x2000001
};
static const SI_RESOURCE_ID_SET gname43[] = {
  0x201
};
static SI_RESOURCE_TOTAL gname42[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */
};
static const mUINT8 latency45[] = {0,0,0,0,0,0};
static const mUINT8 latency46[] = {0,0,0};
static SI gname40 = {
  "BRP",
  3              , /* id */
  latency45      , /* operand latency */
  latency46      , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req41      , /* resource requirement */
  gname43        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname42        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group CHK_ALAT */
static const SI_RRW res_req50[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname52[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname51[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency54[] = {0,0,0,0,0,0};
static const mUINT8 latency55[] = {0,0,0};
static SI gname49 = {
  "CHK_ALAT",
  4              , /* id */
  latency54      , /* operand latency */
  latency55      , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req50      , /* resource requirement */
  gname52        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname51        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group CHK_I */
static const SI_RRW res_req59[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname61[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname60[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency63[] = {0,0,0,0,0,0};
static const mUINT8 latency64[] = {0,0,0};
static SI gname58 = {
  "CHK_I",
  5              , /* id */
  latency63      , /* operand latency */
  latency64      , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req59      , /* resource requirement */
  gname61        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname60        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group CHK_M */
static const SI_RRW res_req68[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname70[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname69[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency72[] = {0,0,0,0,0,0};
static const mUINT8 latency73[] = {0,0,0};
static SI gname67 = {
  "CHK_M",
  6              , /* id */
  latency72      , /* operand latency */
  latency73      , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req68      , /* resource requirement */
  gname70        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname69        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group CLD */
static const SI_RRW res_req77[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname79[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname78[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency81[] = {0,0,0,0,0,0};
static const mUINT8 latency82[] = {0,0,0};
static SI gname76 = {
  "CLD",
  7              , /* id */
  latency81      , /* operand latency */
  latency82      , /* result latency */
  2              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req77      , /* resource requirement */
  gname79        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname78        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FCLD */
static const SI_RRW res_req86[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname88[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname87[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency90[] = {0,0,0,0,0,0};
static const mUINT8 latency91[] = {0,0,0};
static SI gname85 = {
  "FCLD",
  8              , /* id */
  latency90      , /* operand latency */
  latency91      , /* result latency */
  2              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req86      , /* resource requirement */
  gname88        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname87        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FCMP */
static const SI_RRW res_req95[] = {
  1,
  0x48001
};
static const SI_RESOURCE_ID_SET gname97[] = {
  0x61
};
static SI_RESOURCE_TOTAL gname96[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */,
  {&resource15,1} /* floating-point0 */
};
static const mUINT8 latency99[] = {0,0,0,0,0,0};
static const mUINT8 latency100[] = {2,2,2};
static SI gname94 = {
  "FCMP",
  9              , /* id */
  latency99      , /* operand latency */
  latency100     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req95      , /* resource requirement */
  gname97        , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname96        , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FCVTFX */
static const SI_RRW res_req104[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname106[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname105[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency108[] = {0,0,0,0,0,0};
static const mUINT8 latency109[] = {7,7,7};
static SI gname103 = {
  "FCVTFX",
  10             , /* id */
  latency108     , /* operand latency */
  latency109     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req104     , /* resource requirement */
  gname106       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname105       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FLD */
static const SI_RRW res_req113[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname115[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname114[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency117[] = {0,0,0,0,0,0};
static const mUINT8 latency118[] = {9,9,9};
static SI gname112 = {
  "FLD",
  11             , /* id */
  latency117     , /* operand latency */
  latency118     , /* result latency */
  9              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req113     , /* resource requirement */
  gname115       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname114       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FLDP */
static const SI_RRW res_req122[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname124[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname123[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency126[] = {0,0,0,0,0,0};
static const mUINT8 latency127[] = {9,9,9};
static SI gname121 = {
  "FLDP",
  12             , /* id */
  latency126     , /* operand latency */
  latency127     , /* result latency */
  9              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req122     , /* resource requirement */
  gname124       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname123       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FMAC */
static const SI_RRW res_req131[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname133[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname132[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency135[] = {0,0,0,0,0,0};
static const mUINT8 latency136[] = {5,5,5};
static SI gname130 = {
  "FMAC",
  13             , /* id */
  latency135     , /* operand latency */
  latency136     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req131     , /* resource requirement */
  gname133       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname132       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FMISC */
static const SI_RRW res_req140[] = {
  1,
  0x48001
};
static const SI_RESOURCE_ID_SET gname142[] = {
  0x61
};
static SI_RESOURCE_TOTAL gname141[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */,
  {&resource15,1} /* floating-point0 */
};
static const mUINT8 latency144[] = {0,0,0,0,0,0};
static const mUINT8 latency145[] = {5,5,5};
static SI gname139 = {
  "FMISC",
  14             , /* id */
  latency144     , /* operand latency */
  latency145     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req140     , /* resource requirement */
  gname142       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname141       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FOTHER */
static const SI_RRW res_req149[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname151[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname150[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency153[] = {0,0,0,0,0,0};
static const mUINT8 latency154[] = {0,0,0};
static SI gname148 = {
  "FOTHER",
  15             , /* id */
  latency153     , /* operand latency */
  latency154     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req149     , /* resource requirement */
  gname151       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname150       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRAR_I */
static const SI_RRW res_req158[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname160[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname159[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency162[] = {0,0,0,0,0,0};
static const mUINT8 latency163[] = {1,1,1};
static SI gname157 = {
  "FRAR_I",
  16             , /* id */
  latency162     , /* operand latency */
  latency163     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req158     , /* resource requirement */
  gname160       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname159       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRAR_M */
static const SI_RRW res_req167[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname169[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname168[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency171[] = {0,0,0,0,0,0};
static const mUINT8 latency172[] = {1,1,1};
static SI gname166 = {
  "FRAR_M",
  17             , /* id */
  latency171     , /* operand latency */
  latency172     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req167     , /* resource requirement */
  gname169       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname168       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRBR */
static const SI_RRW res_req176[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname178[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname177[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency180[] = {0,0,0,0,0,0};
static const mUINT8 latency181[] = {2,2,2};
static SI gname175 = {
  "FRBR",
  18             , /* id */
  latency180     , /* operand latency */
  latency181     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req176     , /* resource requirement */
  gname178       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname177       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRCR */
static const SI_RRW res_req185[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname187[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname186[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency189[] = {0,0,0,0,0,0};
static const mUINT8 latency190[] = {1,1,1};
static SI gname184 = {
  "FRCR",
  19             , /* id */
  latency189     , /* operand latency */
  latency190     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req185     , /* resource requirement */
  gname187       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname186       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRFR */
static const SI_RRW res_req194[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname196[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname195[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency198[] = {0,0,0,0,0,0};
static const mUINT8 latency199[] = {2,2,2};
static SI gname193 = {
  "FRFR",
  20             , /* id */
  latency198     , /* operand latency */
  latency199     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req194     , /* resource requirement */
  gname196       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname195       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRIP */
static const SI_RRW res_req203[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname205[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname204[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency207[] = {0,0,0,0,0,0};
static const mUINT8 latency208[] = {2,2,2};
static SI gname202 = {
  "FRIP",
  21             , /* id */
  latency207     , /* operand latency */
  latency208     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req203     , /* resource requirement */
  gname205       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname204       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group FRPR */
static const SI_RRW res_req212[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname214[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname213[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency216[] = {0,0,0,0,0,0};
static const mUINT8 latency217[] = {2,2,2};
static SI gname211 = {
  "FRPR",
  22             , /* id */
  latency216     , /* operand latency */
  latency217     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req212     , /* resource requirement */
  gname214       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname213       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group IALU */
static const SI_RRW res_req221[] = {
  1,
  0x41
};
static const SI_RESOURCE_ID_SET gname223[] = {
  0x5
};
static SI_RESOURCE_TOTAL gname222[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */
};
static const mUINT8 latency225[] = {0,0,0,0,0,0};
static const mUINT8 latency226[] = {1,1,1};
static SI gname220 = {
  "IALU",
  23             , /* id */
  latency225     , /* operand latency */
  latency226     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req221     , /* resource requirement */
  gname223       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname222       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group ICMP */
static const SI_RRW res_req230[] = {
  1,
  0x41
};
static const SI_RESOURCE_ID_SET gname232[] = {
  0x5
};
static SI_RESOURCE_TOTAL gname231[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */
};
static const mUINT8 latency234[] = {0,0,0,0,0,0};
static const mUINT8 latency235[] = {1,1,1};
static SI gname229 = {
  "ICMP",
  24             , /* id */
  latency234     , /* operand latency */
  latency235     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req230     , /* resource requirement */
  gname232       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname231       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group ILOG */
static const SI_RRW res_req239[] = {
  1,
  0x41
};
static const SI_RESOURCE_ID_SET gname241[] = {
  0x5
};
static SI_RESOURCE_TOTAL gname240[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */
};
static const mUINT8 latency243[] = {0,0,0,0,0,0};
static const mUINT8 latency244[] = {1,1,1};
static SI gname238 = {
  "ILOG",
  25             , /* id */
  latency243     , /* operand latency */
  latency244     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req239     , /* resource requirement */
  gname241       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname240       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group ISHF */
static const SI_RRW res_req248[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname250[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname249[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency252[] = {0,0,0,0,0,0};
static const mUINT8 latency253[] = {1,1,1};
static SI gname247 = {
  "ISHF",
  26             , /* id */
  latency252     , /* operand latency */
  latency253     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req248     , /* resource requirement */
  gname250       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname249       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group LD */
static const SI_RRW res_req257[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname259[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname258[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency261[] = {0,0,0,0,0,0};
static const mUINT8 latency262[] = {2,2,2};
static SI gname256 = {
  "LD",
  27             , /* id */
  latency261     , /* operand latency */
  latency262     , /* result latency */
  2              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req257     , /* resource requirement */
  gname259       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname258       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group LFETCH */
static const SI_RRW res_req266[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname268[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname267[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency270[] = {0,0,0,0,0,0};
static const mUINT8 latency271[] = {0,0,0};
static SI gname265 = {
  "LFETCH",
  28             , /* id */
  latency270     , /* operand latency */
  latency271     , /* result latency */
  6              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req266     , /* resource requirement */
  gname268       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname267       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group LONG_I */
static const SI_RRW res_req275[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname277[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname276[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency279[] = {0,0,0,0,0,0};
static const mUINT8 latency280[] = {1,1,1};
static SI gname274 = {
  "LONG_I",
  29             , /* id */
  latency279     , /* operand latency */
  latency280     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req275     , /* resource requirement */
  gname277       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname276       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group MMALU_A */
static const SI_RRW res_req284[] = {
  1,
  0x41
};
static const SI_RESOURCE_ID_SET gname286[] = {
  0x5
};
static SI_RESOURCE_TOTAL gname285[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */
};
static const mUINT8 latency288[] = {0,0,0,0,0,0};
static const mUINT8 latency289[] = {2,2,2};
static SI gname283 = {
  "MMALU_A",
  30             , /* id */
  latency288     , /* operand latency */
  latency289     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req284     , /* resource requirement */
  gname286       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname285       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group MMALU_I */
static const SI_RRW res_req293[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname295[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname294[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency297[] = {0,0,0,0,0,0};
static const mUINT8 latency298[] = {2,2,2};
static SI gname292 = {
  "MMALU_I",
  31             , /* id */
  latency297     , /* operand latency */
  latency298     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req293     , /* resource requirement */
  gname295       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname294       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group MMMUL */
static const SI_RRW res_req302[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname304[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname303[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency306[] = {0,0,0,0,0,0};
static const mUINT8 latency307[] = {2,2,2};
static SI gname301 = {
  "MMMUL",
  32             , /* id */
  latency306     , /* operand latency */
  latency307     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req302     , /* resource requirement */
  gname304       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname303       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group MMSHF */
static const SI_RRW res_req311[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname313[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname312[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency315[] = {0,0,0,0,0,0};
static const mUINT8 latency316[] = {2,2,2};
static SI gname310 = {
  "MMSHF",
  33             , /* id */
  latency315     , /* operand latency */
  latency316     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req311     , /* resource requirement */
  gname313       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname312       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group NOP_B */
static const SI_RRW res_req320[] = {
  1,
  0x2000001
};
static const SI_RESOURCE_ID_SET gname322[] = {
  0x201
};
static SI_RESOURCE_TOTAL gname321[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */
};
static const mUINT8 latency324[] = {0,0,0,0,0,0};
static const mUINT8 latency325[] = {0,0,0};
static SI gname319 = {
  "NOP_B",
  34             , /* id */
  latency324     , /* operand latency */
  latency325     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req320     , /* resource requirement */
  gname322       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname321       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group NOP_I */
static const SI_RRW res_req329[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname331[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname330[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency333[] = {0,0,0,0,0,0};
static const mUINT8 latency334[] = {0,0,0};
static SI gname328 = {
  "NOP_I",
  35             , /* id */
  latency333     , /* operand latency */
  latency334     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req329     , /* resource requirement */
  gname331       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname330       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group NOP_M */
static const SI_RRW res_req338[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname340[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname339[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency342[] = {0,0,0,0,0,0};
static const mUINT8 latency343[] = {0,0,0};
static SI gname337 = {
  "NOP_M",
  36             , /* id */
  latency342     , /* operand latency */
  latency343     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req338     , /* resource requirement */
  gname340       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname339       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group NOP_F */
static const SI_RRW res_req347[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname349[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname348[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency351[] = {0,0,0,0,0,0};
static const mUINT8 latency352[] = {0,0,0};
static SI gname346 = {
  "NOP_F",
  37             , /* id */
  latency351     , /* operand latency */
  latency352     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req347     , /* resource requirement */
  gname349       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname348       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group NOP_X */
static const SI_RRW res_req356[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname358[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname357[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency360[] = {0,0,0,0,0,0};
static const mUINT8 latency361[] = {0,0,0};
static SI gname355 = {
  "NOP_X",
  38             , /* id */
  latency360     , /* operand latency */
  latency361     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req356     , /* resource requirement */
  gname358       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname357       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group PNT */
static const SI_RRW res_req365[] = {
  1,
  0x41
};
static const SI_RESOURCE_ID_SET gname367[] = {
  0x5
};
static SI_RESOURCE_TOTAL gname366[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */
};
static const mUINT8 latency369[] = {0,0,0,0,0,0};
static const mUINT8 latency370[] = {1,1,1};
static SI gname364 = {
  "PNT",
  39             , /* id */
  latency369     , /* operand latency */
  latency370     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req365     , /* resource requirement */
  gname367       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname366       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group RSE_B */
static const SI_RRW res_req374[] = {
  1,
  0x2000001
};
static const SI_RESOURCE_ID_SET gname376[] = {
  0x201
};
static SI_RESOURCE_TOTAL gname375[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */
};
static const mUINT8 latency378[] = {0,0,0,0,0,0};
static const mUINT8 latency379[] = {0,0,0};
static SI gname373 = {
  "RSE_B",
  40             , /* id */
  latency378     , /* operand latency */
  latency379     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req374     , /* resource requirement */
  gname376       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname375       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group RSE_M */
static const SI_RRW res_req383[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname385[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname384[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency387[] = {0,0,0,0,0,0};
static const mUINT8 latency388[] = {0,0,0};
static SI gname382 = {
  "RSE_M",
  41             , /* id */
  latency387     , /* operand latency */
  latency388     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req383     , /* resource requirement */
  gname385       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname384       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SEM */
static const SI_RRW res_req392[] = {
  11,
  0x2451,
  0x10,
  0x10,
  0x10,
  0x10,
  0x10,
  0x10,
  0x10,
  0x10,
  0x10,
  0x10
};
static const SI_RESOURCE_ID_SET gname394[] = {
  0x1f,
  0x2,
  0x2,
  0x2,
  0x2,
  0x2,
  0x2,
  0x2,
  0x2,
  0x2,
  0x2
};
static SI_RESOURCE_TOTAL gname393[] = {
  {&resource9,1} /* issue */,
  {&resource10,11} /* sem */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const SI_RR ii_rr398[] = {
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
};
static const SI_RESOURCE_ID_SET * const gname399[] = {
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
};
static const mUINT8 latency396[] = {0,0,0,0,0,0};
static const mUINT8 latency397[] = {11,11,11};
static SI gname391 = {
  "SEM",
  42             , /* id */
  latency396     , /* operand latency */
  latency397     , /* result latency */
  11             , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req392     , /* resource requirement */
  gname394       , /* res id used set vec */
  10             , /* II info size */
  ii_rr398       , /* II resource requirement vec */
  gname399       , /* II res id used set vec */
  {{0x3ff,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  5              , /* resource count vec size */
  gname393       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SFCVTFX */
static const SI_RRW res_req401[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname403[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname402[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency405[] = {0,0,0,0,0,0};
static const mUINT8 latency406[] = {7,7,7};
static SI gname400 = {
  "SFCVTFX",
  43             , /* id */
  latency405     , /* operand latency */
  latency406     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req401     , /* resource requirement */
  gname403       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname402       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SFMAC */
static const SI_RRW res_req410[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname412[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname411[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency414[] = {0,0,0,0,0,0};
static const mUINT8 latency415[] = {5,5,5};
static SI gname409 = {
  "SFMAC",
  44             , /* id */
  latency414     , /* operand latency */
  latency415     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req410     , /* resource requirement */
  gname412       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname411       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SFMERGESE */
static const SI_RRW res_req419[] = {
  1,
  0x48001
};
static const SI_RESOURCE_ID_SET gname421[] = {
  0x61
};
static SI_RESOURCE_TOTAL gname420[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */,
  {&resource15,1} /* floating-point0 */
};
static const mUINT8 latency423[] = {0,0,0,0,0,0};
static const mUINT8 latency424[] = {7,7,7};
static SI gname418 = {
  "SFMERGESE",
  45             , /* id */
  latency423     , /* operand latency */
  latency424     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req419     , /* resource requirement */
  gname421       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname420       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SFMISC */
static const SI_RRW res_req428[] = {
  1,
  0x48001
};
static const SI_RESOURCE_ID_SET gname430[] = {
  0x61
};
static SI_RESOURCE_TOTAL gname429[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */,
  {&resource15,1} /* floating-point0 */
};
static const mUINT8 latency432[] = {0,0,0,0,0,0};
static const mUINT8 latency433[] = {5,5,5};
static SI gname427 = {
  "SFMISC",
  46             , /* id */
  latency432     , /* operand latency */
  latency433     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req428     , /* resource requirement */
  gname430       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname429       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group STF */
static const SI_RRW res_req437[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname439[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname438[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency441[] = {0,0,0,0,0,0};
static const mUINT8 latency442[] = {0,0,0};
static SI gname436 = {
  "STF",
  47             , /* id */
  latency441     , /* operand latency */
  latency442     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  9              , /* store available time */
  res_req437     , /* resource requirement */
  gname439       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname438       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group ST */
static const SI_RRW res_req446[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname448[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname447[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency450[] = {0,0,0,0,0,0};
static const mUINT8 latency451[] = {0,0,0};
static SI gname445 = {
  "ST",
  48             , /* id */
  latency450     , /* operand latency */
  latency451     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  2              , /* store available time */
  res_req446     , /* resource requirement */
  gname448       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname447       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_B2 */
static const SI_RRW res_req455[] = {
  1,
  0x82000001
};
static const SI_RESOURCE_ID_SET gname457[] = {
  0xa01
};
static SI_RESOURCE_TOTAL gname456[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */,
  {&resource20,1} /* B0_or_B2 */
};
static const mUINT8 latency459[] = {0,0,0,0,0,0};
static const mUINT8 latency460[] = {0,0,0};
static SI gname454 = {
  "SYST_B2",
  49             , /* id */
  latency459     , /* operand latency */
  latency460     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req455     , /* resource requirement */
  gname457       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname456       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_B */
static const SI_RRW res_req464[] = {
  1,
  0x12000001
};
static const SI_RESOURCE_ID_SET gname466[] = {
  0x601
};
static SI_RESOURCE_TOTAL gname465[] = {
  {&resource9,1} /* issue */,
  {&resource18,1} /* branch */,
  {&resource19,1} /* B0_or_B1 */
};
static const mUINT8 latency468[] = {0,0,0,0,0,0};
static const mUINT8 latency469[] = {0,0,0};
static SI gname463 = {
  "SYST_B",
  50             , /* id */
  latency468     , /* operand latency */
  latency469     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req464     , /* resource requirement */
  gname466       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname465       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_0 */
static const SI_RRW res_req473[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname475[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname474[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency477[] = {0,0,0,0,0,0};
static const mUINT8 latency478[] = {0,0,0};
static SI gname472 = {
  "SYST_M0_0",
  51             , /* id */
  latency477     , /* operand latency */
  latency478     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req473     , /* resource requirement */
  gname475       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname474       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_1 */
static const SI_RRW res_req482[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname484[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname483[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency486[] = {0,0,0,0,0,0};
static const mUINT8 latency487[] = {1,1,1};
static SI gname481 = {
  "SYST_M0_1",
  52             , /* id */
  latency486     , /* operand latency */
  latency487     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req482     , /* resource requirement */
  gname484       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname483       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_2 */
static const SI_RRW res_req491[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname493[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname492[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency495[] = {0,0,0,0,0,0};
static const mUINT8 latency496[] = {1,1,1};
static SI gname490 = {
  "SYST_M0_2",
  53             , /* id */
  latency495     , /* operand latency */
  latency496     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req491     , /* resource requirement */
  gname493       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname492       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_3 */
static const SI_RRW res_req500[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname502[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname501[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency504[] = {0,0,0,0,0,0};
static const mUINT8 latency505[] = {4,4,4};
static SI gname499 = {
  "SYST_M0_3",
  54             , /* id */
  latency504     , /* operand latency */
  latency505     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req500     , /* resource requirement */
  gname502       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname501       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_4 */
static const SI_RRW res_req509[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname511[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname510[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency513[] = {0,0,0,0,0,0};
static const mUINT8 latency514[] = {5,5,5};
static SI gname508 = {
  "SYST_M0_4",
  55             , /* id */
  latency513     , /* operand latency */
  latency514     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req509     , /* resource requirement */
  gname511       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname510       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_5 */
static const SI_RRW res_req518[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname520[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname519[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency522[] = {0,0,0,0,0,0};
static const mUINT8 latency523[] = {6,6,6};
static SI gname517 = {
  "SYST_M0_5",
  56             , /* id */
  latency522     , /* operand latency */
  latency523     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req518     , /* resource requirement */
  gname520       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname519       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_6 */
static const SI_RRW res_req527[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname529[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname528[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency531[] = {0,0,0,0,0,0};
static const mUINT8 latency532[] = {10,10,10};
static SI gname526 = {
  "SYST_M0_6",
  57             , /* id */
  latency531     , /* operand latency */
  latency532     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req527     , /* resource requirement */
  gname529       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname528       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_7 */
static const SI_RRW res_req536[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname538[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname537[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency540[] = {0,0,0,0,0,0};
static const mUINT8 latency541[] = {13,13,13};
static SI gname535 = {
  "SYST_M0_7",
  58             , /* id */
  latency540     , /* operand latency */
  latency541     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req536     , /* resource requirement */
  gname538       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname537       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_8 */
static const SI_RRW res_req545[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname547[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname546[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency549[] = {0,0,0,0,0,0};
static const mUINT8 latency550[] = {35,35,35};
static SI gname544 = {
  "SYST_M0_8",
  59             , /* id */
  latency549     , /* operand latency */
  latency550     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req545     , /* resource requirement */
  gname547       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname546       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M0_9 */
static const SI_RRW res_req554[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname556[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname555[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency558[] = {0,0,0,0,0,0};
static const mUINT8 latency559[] = {38,38,38};
static SI gname553 = {
  "SYST_M0_9",
  60             , /* id */
  latency558     , /* operand latency */
  latency559     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req554     , /* resource requirement */
  gname556       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname555       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group SYST_M */
static const SI_RRW res_req563[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname565[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname564[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency567[] = {0,0,0,0,0,0};
static const mUINT8 latency568[] = {0,0,0};
static SI gname562 = {
  "SYST_M",
  61             , /* id */
  latency567     , /* operand latency */
  latency568     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req563     , /* resource requirement */
  gname565       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname564       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TBIT */
static const SI_RRW res_req572[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname574[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname573[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency576[] = {0,0,0,0,0,0};
static const mUINT8 latency577[] = {1,1,1};
static SI gname571 = {
  "TBIT",
  62             , /* id */
  latency576     , /* operand latency */
  latency577     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req572     , /* resource requirement */
  gname574       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname573       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TOAR_I */
static const SI_RRW res_req581[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname583[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname582[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency585[] = {0,0,0,0,0,0};
static const mUINT8 latency586[] = {1,1,1};
static SI gname580 = {
  "TOAR_I",
  63             , /* id */
  latency585     , /* operand latency */
  latency586     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req581     , /* resource requirement */
  gname583       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname582       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TOAR_M */
static const SI_RRW res_req590[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname592[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname591[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency594[] = {0,0,0,0,0,0};
static const mUINT8 latency595[] = {1,1,1};
static SI gname589 = {
  "TOAR_M",
  64             , /* id */
  latency594     , /* operand latency */
  latency595     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req590     , /* resource requirement */
  gname592       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname591       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TOBR */
static const SI_RRW res_req599[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname601[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname600[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency603[] = {0,0,0,0,0,0};
static const mUINT8 latency604[] = {1,1,1};
static SI gname598 = {
  "TOBR",
  65             , /* id */
  latency603     , /* operand latency */
  latency604     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req599     , /* resource requirement */
  gname601       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname600       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TOCR */
static const SI_RRW res_req608[] = {
  1,
  0x2441
};
static const SI_RESOURCE_ID_SET gname610[] = {
  0x1d
};
static SI_RESOURCE_TOTAL gname609[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */,
  {&resource13,1} /* memory0 */
};
static const mUINT8 latency612[] = {0,0,0,0,0,0};
static const mUINT8 latency613[] = {1,1,1};
static SI gname607 = {
  "TOCR",
  66             , /* id */
  latency612     , /* operand latency */
  latency613     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req608     , /* resource requirement */
  gname610       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname609       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TOFR */
static const SI_RRW res_req617[] = {
  1,
  0x441
};
static const SI_RESOURCE_ID_SET gname619[] = {
  0xd
};
static SI_RESOURCE_TOTAL gname618[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource12,1} /* memory */
};
static const mUINT8 latency621[] = {0,0,0,0,0,0};
static const mUINT8 latency622[] = {9,9,9};
static SI gname616 = {
  "TOFR",
  67             , /* id */
  latency621     , /* operand latency */
  latency622     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req617     , /* resource requirement */
  gname619       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname618       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group TOPR */
static const SI_RRW res_req626[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname628[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname627[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency630[] = {0,0,0,0,0,0};
static const mUINT8 latency631[] = {1,1,1};
static SI gname625 = {
  "TOPR",
  68             , /* id */
  latency630     , /* operand latency */
  latency631     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req626     , /* resource requirement */
  gname628       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname627       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group XMA */
static const SI_RRW res_req635[] = {
  1,
  0x8001
};
static const SI_RESOURCE_ID_SET gname637[] = {
  0x21
};
static SI_RESOURCE_TOTAL gname636[] = {
  {&resource9,1} /* issue */,
  {&resource14,1} /* floating-point */
};
static const mUINT8 latency639[] = {0,0,0,0,0,0};
static const mUINT8 latency640[] = {7,7,7};
static SI gname634 = {
  "XMA",
  69             , /* id */
  latency639     , /* operand latency */
  latency640     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req635     , /* resource requirement */
  gname637       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  2              , /* resource count vec size */
  gname636       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group XTD */
static const SI_RRW res_req644[] = {
  1,
  0x100041
};
static const SI_RESOURCE_ID_SET gname646[] = {
  0x85
};
static SI_RESOURCE_TOTAL gname645[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */
};
static const mUINT8 latency648[] = {0,0,0,0,0,0};
static const mUINT8 latency649[] = {1,1,1};
static SI gname643 = {
  "XTD",
  70             , /* id */
  latency648     , /* operand latency */
  latency649     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req644     , /* resource requirement */
  gname646       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  3              , /* resource count vec size */
  gname645       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group UNKNOWN_1 */
static const SI_RRW res_req653[] = {
  1,
  0x900041
};
static const SI_RESOURCE_ID_SET gname655[] = {
  0x185
};
static SI_RESOURCE_TOTAL gname654[] = {
  {&resource9,1} /* issue */,
  {&resource11,1} /* integer_or_memory */,
  {&resource16,1} /* integer */,
  {&resource17,1} /* integer0 */
};
static const mUINT8 latency657[] = {0,0,0,0,0,0};
static const mUINT8 latency658[] = {1,1,1};
static SI gname652 = {
  "UNKNOWN_1",
  71             , /* id */
  latency657     , /* operand latency */
  latency658     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req653     , /* resource requirement */
  gname655       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  4              , /* resource count vec size */
  gname654       , /* resource count vec */
  0                /* write-write interlock */
};

/* Instruction group dummy */
static const SI_RRW res_req662[] = {
  1,
  0x1
};
static const SI_RESOURCE_ID_SET gname664[] = {
  0x1
};
static SI_RESOURCE_TOTAL gname663[] = {
  {&resource9,1} /* issue */
};
static const mUINT8 latency666[] = {0,0,0,0,0,0};
static const mUINT8 latency667[] = {1,1,1};
static SI gname661 = {
  "dummy",
  72             , /* id */
  latency666     , /* operand latency */
  latency667     , /* result latency */
  0              , /* load access time */
  0              , /* last issue cycle */
  0              , /* store available time */
  res_req662     , /* resource requirement */
  gname664       , /* res id used set vec */
  0              , /* II info size */
  0              , /* II resource requirement vec */
  0              , /* II res id used set vec */
  {{0x0,0x0}}    , /* Bad IIs */
  0              , /* valid issue slots vec size */
  0              , /* valid issue slots vec */
  1              , /* resource count vec size */
  gname663       , /* resource count vec */
  0                /* write-write interlock */
};
SI * const SI_ID_si[] = {
  &gname0,
  &gname22,
  &gname31,
  &gname40,
  &gname49,
  &gname58,
  &gname67,
  &gname76,
  &gname85,
  &gname94,
  &gname103,
  &gname112,
  &gname121,
  &gname130,
  &gname139,
  &gname148,
  &gname157,
  &gname166,
  &gname175,
  &gname184,
  &gname193,
  &gname202,
  &gname211,
  &gname220,
  &gname229,
  &gname238,
  &gname247,
  &gname256,
  &gname265,
  &gname274,
  &gname283,
  &gname292,
  &gname301,
  &gname310,
  &gname319,
  &gname328,
  &gname337,
  &gname346,
  &gname355,
  &gname364,
  &gname373,
  &gname382,
  &gname391,
  &gname400,
  &gname409,
  &gname418,
  &gname427,
  &gname436,
  &gname445,
  &gname454,
  &gname463,
  &gname472,
  &gname481,
  &gname490,
  &gname499,
  &gname508,
  &gname517,
  &gname526,
  &gname535,
  &gname544,
  &gname553,
  &gname562,
  &gname571,
  &gname580,
  &gname589,
  &gname598,
  &gname607,
  &gname616,
  &gname625,
  &gname634,
  &gname643,
  &gname652,
  &gname661
};
const int SI_ID_count = 73;

SI * const SI_top_si[759] = {
  &gname220   /* add */,
  &gname220   /* add_1 */,
  &gname220   /* adds */,
  &gname220   /* addl */,
  &gname364   /* addp4 */,
  &gname364   /* addp4_i */,
  &gname481   /* alloc_3 */,
  &gname238   /* and */,
  &gname238   /* and_i */,
  &gname238   /* andcm */,
  &gname238   /* andcm_i */,
  &gname22    /* br.cond */,
  &gname22    /* br_r.cond */,
  &gname22    /* br.call */,
  &gname22    /* br_r.call */,
  &gname22    /* br.ret */,
  &gname22    /* br.ia */,
  &gname31    /* br.cloop */,
  &gname31    /* br.ctop */,
  &gname31    /* br.cexit */,
  &gname31    /* br.wtop */,
  &gname31    /* br.wexit */,
  &gname328   /* break.i */,
  &gname319   /* break.b */,
  &gname337   /* break.m */,
  &gname346   /* break.f */,
  &gname355   /* break.x */,
  &gname22    /* brl.cond */,
  &gname22    /* brl.call */,
  &gname40    /* brp */,
  &gname40    /* brp_r */,
  &gname40    /* brp.ret */,
  &gname454   /* bsw.0 */,
  &gname454   /* bsw.1 */,
  &gname58    /* chk.s.i */,
  &gname67    /* chk.s.m */,
  &gname67    /* chk_f.s */,
  &gname49    /* chk.a */,
  &gname49    /* chk_f.a */,
  &gname373   /* clrrrb */,
  &gname373   /* clrrrb.pr */,
  &gname229   /* cmp.eq */,
  &gname229   /* cmp.eq.unc */,
  &gname229   /* cmp.eq.and */,
  &gname229   /* cmp.eq.or */,
  &gname229   /* cmp.eq.or.andcm */,
  &gname229   /* cmp.ne.and */,
  &gname229   /* cmp.ne.or */,
  &gname229   /* cmp.ne.or.andcm */,
  &gname229   /* cmp.lt */,
  &gname229   /* cmp.lt.unc */,
  &gname229   /* cmp.ltu */,
  &gname229   /* cmp.ltu.unc */,
  &gname229   /* cmp_z1.lt.and */,
  &gname229   /* cmp_z1.lt.or */,
  &gname229   /* cmp_z1.lt.or.andcm */,
  &gname229   /* cmp_z1.le.and */,
  &gname229   /* cmp_z1.le.or */,
  &gname229   /* cmp_z1.le.or.andcm */,
  &gname229   /* cmp_z1.gt.and */,
  &gname229   /* cmp_z1.gt.or */,
  &gname229   /* cmp_z1.gt.or.andcm */,
  &gname229   /* cmp_z1.ge.and */,
  &gname229   /* cmp_z1.ge.or */,
  &gname229   /* cmp_z1.ge.or.andcm */,
  &gname229   /* cmp_i.eq */,
  &gname229   /* cmp_i.eq.unc */,
  &gname229   /* cmp_i.eq.and */,
  &gname229   /* cmp_i.eq.or */,
  &gname229   /* cmp_i.eq.or.andcm */,
  &gname229   /* cmp_i.ne.and */,
  &gname229   /* cmp_i.ne.or */,
  &gname229   /* cmp_i.ne.or.andcm */,
  &gname229   /* cmp_i.lt */,
  &gname229   /* cmp_i.lt.unc */,
  &gname229   /* cmp_i.ltu */,
  &gname229   /* cmp_i.ltu.unc */,
  &gname229   /* cmp4.eq */,
  &gname229   /* cmp4.eq.unc */,
  &gname229   /* cmp4.eq.and */,
  &gname229   /* cmp4.eq.or */,
  &gname229   /* cmp4.eq.or.andcm */,
  &gname229   /* cmp4.ne.and */,
  &gname229   /* cmp4.ne.or */,
  &gname229   /* cmp4.ne.or.andcm */,
  &gname229   /* cmp4.lt */,
  &gname229   /* cmp4.lt.unc */,
  &gname229   /* cmp4.ltu */,
  &gname229   /* cmp4.ltu.unc */,
  &gname229   /* cmp4_z1.lt.and */,
  &gname229   /* cmp4_z1.lt.or */,
  &gname229   /* cmp4_z1.lt.or.andcm */,
  &gname229   /* cmp4_z1.le.and */,
  &gname229   /* cmp4_z1.le.or */,
  &gname229   /* cmp4_z1.le.or.andcm */,
  &gname229   /* cmp4_z1.gt.and */,
  &gname229   /* cmp4_z1.gt.or */,
  &gname229   /* cmp4_z1.gt.or.andcm */,
  &gname229   /* cmp4_z1.ge.and */,
  &gname229   /* cmp4_z1.ge.or */,
  &gname229   /* cmp4_z1.ge.or.andcm */,
  &gname229   /* cmp4_i.eq */,
  &gname229   /* cmp4_i.eq.unc */,
  &gname229   /* cmp4_i.eq.and */,
  &gname229   /* cmp4_i.eq.or */,
  &gname229   /* cmp4_i.eq.or.andcm */,
  &gname229   /* cmp4_i.ne.and */,
  &gname229   /* cmp4_i.ne.or */,
  &gname229   /* cmp4_i.ne.or.andcm */,
  &gname229   /* cmp4_i.lt */,
  &gname229   /* cmp4_i.lt.unc */,
  &gname229   /* cmp4_i.ltu */,
  &gname229   /* cmp4_i.ltu.unc */,
  &gname391   /* cmpxchg1 */,
  &gname391   /* cmpxchg2 */,
  &gname391   /* cmpxchg4 */,
  &gname391   /* cmpxchg8 */,
  &gname373   /* cover */,
  &gname643   /* czx1.l */,
  &gname643   /* czx1.r */,
  &gname643   /* czx2.l */,
  &gname643   /* czx2.r */,
  &gname247   /* dep */,
  &gname247   /* dep.z */,
  &gname247   /* dep_i */,
  &gname247   /* dep_i.z */,
  &gname463   /* epc */,
  &gname247   /* extr */,
  &gname247   /* extr.u */,
  &gname139   /* famax */,
  &gname139   /* famin */,
  &gname139   /* fand */,
  &gname139   /* fandcm */,
  &gname490   /* fc */,
  &gname148   /* fchkf */,
  &gname94    /* fclass.m */,
  &gname94    /* fclass.m.unc */,
  &gname148   /* fclrf */,
  &gname94    /* fcmp.eq */,
  &gname94    /* fcmp.eq.unc */,
  &gname94    /* fcmp.lt */,
  &gname94    /* fcmp.lt.unc */,
  &gname94    /* fcmp.le */,
  &gname94    /* fcmp.le.unc */,
  &gname94    /* fcmp.unord */,
  &gname94    /* fcmp.unord.unc */,
  &gname103   /* fcvt.fx */,
  &gname103   /* fcvt.fx.trunc */,
  &gname103   /* fcvt.fxu */,
  &gname103   /* fcvt.fxu.trunc */,
  &gname103   /* fcvt.xf */,
  &gname391   /* fetchadd4 */,
  &gname391   /* fetchadd8 */,
  &gname382   /* flushrs */,
  &gname130   /* fma */,
  &gname130   /* fma.s */,
  &gname130   /* fma.d */,
  &gname139   /* fmax */,
  &gname139   /* fmerge.ns */,
  &gname139   /* fmerge.s */,
  &gname139   /* fmerge.se */,
  &gname139   /* fmin */,
  &gname139   /* fmix.l */,
  &gname139   /* fmix.r */,
  &gname139   /* fmix.lr */,
  &gname130   /* fms */,
  &gname130   /* fms.s */,
  &gname130   /* fms.d */,
  &gname130   /* fnma */,
  &gname130   /* fnma.s */,
  &gname130   /* fnma.d */,
  &gname139   /* for */,
  &gname139   /* fpack */,
  &gname427   /* fpamax */,
  &gname427   /* fpamin */,
  &gname427   /* fpcmp.eq */,
  &gname427   /* fpcmp.lt */,
  &gname427   /* fpcmp.le */,
  &gname427   /* fpcmp.unord */,
  &gname427   /* fpcmp.neq */,
  &gname427   /* fpcmp.nlt */,
  &gname427   /* fpcmp.nle */,
  &gname427   /* fpcmp.ord */,
  &gname400   /* fpcvt.fx */,
  &gname400   /* fpcvt.fx.trunc */,
  &gname400   /* fpcvt.fxu */,
  &gname400   /* fpcvt.fxu.trunc */,
  &gname409   /* fpma */,
  &gname427   /* fpmax */,
  &gname427   /* fpmerge.ns */,
  &gname427   /* fpmerge.s */,
  &gname418   /* fpmerge.se */,
  &gname427   /* fpmin */,
  &gname409   /* fpms */,
  &gname409   /* fpnma */,
  &gname427   /* fprcpa */,
  &gname427   /* fprsqrta */,
  &gname139   /* frcpa */,
  &gname139   /* frsqrta */,
  &gname139   /* fselect */,
  &gname148   /* fsetc */,
  &gname139   /* fswap */,
  &gname139   /* fswap.nl */,
  &gname139   /* fswap.nr */,
  &gname139   /* fsxt.l */,
  &gname139   /* fsxt.r */,
  &gname562   /* fwb */,
  &gname139   /* fxor */,
  &gname193   /* getf.s */,
  &gname193   /* getf.d */,
  &gname193   /* getf.exp */,
  &gname193   /* getf.sig */,
  &gname562   /* invala */,
  &gname562   /* invala.e */,
  &gname562   /* invala_f.e */,
  &gname490   /* itc.i */,
  &gname490   /* itc.d */,
  &gname490   /* itr.i */,
  &gname490   /* itr.d */,
  &gname256   /* ld1 */,
  &gname256   /* ld1_r */,
  &gname256   /* ld1_i */,
  &gname256   /* ld2 */,
  &gname256   /* ld2_r */,
  &gname256   /* ld2_i */,
  &gname256   /* ld4 */,
  &gname256   /* ld4_r */,
  &gname256   /* ld4_i */,
  &gname256   /* ld8 */,
  &gname256   /* ld8_r */,
  &gname256   /* ld8_i */,
  &gname256   /* ld8.fill */,
  &gname256   /* ld8_r.fill */,
  &gname256   /* ld8_i.fill */,
  &gname112   /* ldfs */,
  &gname112   /* ldfs_r */,
  &gname112   /* ldfs_i */,
  &gname112   /* ldfd */,
  &gname112   /* ldfd_r */,
  &gname112   /* ldfd_i */,
  &gname112   /* ldfe */,
  &gname112   /* ldfe_r */,
  &gname112   /* ldfe_i */,
  &gname112   /* ldf8 */,
  &gname112   /* ldf8_r */,
  &gname112   /* ldf8_i */,
  &gname112   /* ldf.fill */,
  &gname112   /* ldf_r.fill */,
  &gname112   /* ldf_i.fill */,
  &gname121   /* ldfps */,
  &gname121   /* ldfps_i */,
  &gname121   /* ldfpd */,
  &gname121   /* ldfpd_i */,
  &gname121   /* ldfp8 */,
  &gname121   /* ldfp8_i */,
  &gname265   /* lfetch */,
  &gname265   /* lfetch.excl */,
  &gname265   /* lfetch.fault */,
  &gname265   /* lfetch.fault.excl */,
  &gname265   /* lfetch_r */,
  &gname265   /* lfetch_r.excl */,
  &gname265   /* lfetch_r.fault */,
  &gname265   /* lfetch_r.fault.excl */,
  &gname265   /* lfetch_i */,
  &gname265   /* lfetch_i.excl */,
  &gname265   /* lfetch_i.fault */,
  &gname265   /* lfetch_i.fault.excl */,
  &gname382   /* loadrs */,
  &gname562   /* mf */,
  &gname472   /* mf.a */,
  &gname310   /* mix1.l */,
  &gname310   /* mix1.r */,
  &gname310   /* mix2.l */,
  &gname310   /* mix2.r */,
  &gname310   /* mix4.l */,
  &gname310   /* mix4.r */,
  &gname157   /* mov_f_ar.i */,
  &gname580   /* mov_t_ar_r.i */,
  &gname580   /* mov_t_ar_i.i */,
  &gname166   /* mov_f_ar.m */,
  &gname589   /* mov_t_ar_r.m */,
  &gname589   /* mov_t_ar_i.m */,
  &gname175   /* mov_f_br */,
  &gname598   /* mov_t_br_i */,
  &gname598   /* mov_t_br.ret */,
  &gname607   /* mov_t_cr */,
  &gname184   /* mov_f_cr */,
  &gname553   /* mov_f_cpuid */,
  &gname544   /* mov_t_dbr */,
  &gname553   /* mov_f_dbr */,
  &gname544   /* mov_t_ibr */,
  &gname553   /* mov_f_ibr */,
  &gname544   /* mov_t_msr */,
  &gname553   /* mov_f_msr */,
  &gname526   /* mov_t_pkr */,
  &gname535   /* mov_f_pkr */,
  &gname544   /* mov_t_pmc */,
  &gname553   /* mov_f_pmc */,
  &gname544   /* mov_t_pmd */,
  &gname553   /* mov_f_pmd */,
  &gname526   /* mov_t_rr */,
  &gname535   /* mov_f_rr */,
  &gname202   /* mov_f_ip */,
  &gname211   /* mov_f_pr */,
  &gname625   /* mov_t_pr */,
  &gname625   /* mov_t_pr_i */,
  &gname508   /* mov_t_psr */,
  &gname535   /* mov_f_psr */,
  &gname499   /* mov_t_psrum */,
  &gname535   /* mov_f_psrum */,
  &gname274   /* movl */,
  &gname310   /* mux1 */,
  &gname310   /* mux2 */,
  &gname328   /* nop.i */,
  &gname319   /* nop.b */,
  &gname337   /* nop.m */,
  &gname346   /* nop.f */,
  &gname355   /* nop.x */,
  &gname238   /* or */,
  &gname238   /* or_i */,
  &gname310   /* pack2.sss */,
  &gname310   /* pack2.uss */,
  &gname310   /* pack4.sss */,
  &gname283   /* padd1 */,
  &gname283   /* padd1.sss */,
  &gname283   /* padd1.uus */,
  &gname283   /* padd1.uuu */,
  &gname283   /* padd2 */,
  &gname283   /* padd2.sss */,
  &gname283   /* padd2.uus */,
  &gname283   /* padd2.uuu */,
  &gname283   /* padd4 */,
  &gname283   /* pavg1 */,
  &gname283   /* pavg1.raz */,
  &gname283   /* pavg2 */,
  &gname283   /* pavg2.raz */,
  &gname283   /* pavgsub1 */,
  &gname283   /* pavgsub2 */,
  &gname283   /* pcmp1.eq */,
  &gname283   /* pcmp1.gt */,
  &gname283   /* pcmp2.eq */,
  &gname283   /* pcmp2.gt */,
  &gname283   /* pcmp4.eq */,
  &gname283   /* pcmp4.gt */,
  &gname292   /* pmax1.u */,
  &gname292   /* pmax2 */,
  &gname292   /* pmin1.u */,
  &gname292   /* pmin2 */,
  &gname301   /* pmpy2.r */,
  &gname301   /* pmpy2.l */,
  &gname301   /* pmpyshr2 */,
  &gname301   /* pmpyshr2.u */,
  &gname301   /* popcnt */,
  &gname490   /* probe.r */,
  &gname490   /* probe.w */,
  &gname490   /* probe_i.r */,
  &gname490   /* probe_i.w */,
  &gname490   /* probe.r.fault */,
  &gname490   /* probe.w.fault */,
  &gname490   /* probe.rw.fault */,
  &gname292   /* psad1 */,
  &gname310   /* pshl2 */,
  &gname310   /* pshl4 */,
  &gname310   /* pshl2_i */,
  &gname310   /* pshl4_i */,
  &gname283   /* pshladd2 */,
  &gname310   /* pshr2 */,
  &gname310   /* pshr2.u */,
  &gname310   /* pshr4 */,
  &gname310   /* pshr4.u */,
  &gname310   /* pshr2_i */,
  &gname310   /* pshr2_i.u */,
  &gname310   /* pshr4_i */,
  &gname310   /* pshr4_i.u */,
  &gname283   /* pshradd2 */,
  &gname283   /* psub1 */,
  &gname283   /* psub2 */,
  &gname283   /* psub4 */,
  &gname283   /* psub1.sss */,
  &gname283   /* psub1.uus */,
  &gname283   /* psub1.uuu */,
  &gname283   /* psub2.sss */,
  &gname283   /* psub2.uus */,
  &gname283   /* psub2.uuu */,
  &gname490   /* ptc.e */,
  &gname490   /* ptc.g */,
  &gname490   /* ptc.ga */,
  &gname490   /* ptc.l */,
  &gname490   /* ptr.d */,
  &gname490   /* ptr.i */,
  &gname454   /* rfi */,
  &gname508   /* rsm */,
  &gname499   /* rum */,
  &gname616   /* setf.s */,
  &gname616   /* setf.d */,
  &gname616   /* setf.exp */,
  &gname616   /* setf.sig */,
  &gname310   /* shl */,
  &gname220   /* shladd */,
  &gname364   /* shladdp4 */,
  &gname310   /* shr */,
  &gname310   /* shr.u */,
  &gname247   /* shrp */,
  &gname562   /* srlz.i */,
  &gname562   /* srlz.d */,
  &gname508   /* ssm */,
  &gname445   /* st1 */,
  &gname445   /* st1_i */,
  &gname445   /* st2 */,
  &gname445   /* st2_i */,
  &gname445   /* st4 */,
  &gname445   /* st4_i */,
  &gname445   /* st8 */,
  &gname445   /* st8_i */,
  &gname445   /* st8.spill */,
  &gname445   /* st8_i.spill */,
  &gname436   /* stfs */,
  &gname436   /* stfs_i */,
  &gname436   /* stfd */,
  &gname436   /* stfd_i */,
  &gname436   /* stfe */,
  &gname436   /* stfe_i */,
  &gname436   /* stf8 */,
  &gname436   /* stf8_i */,
  &gname436   /* stf.spill */,
  &gname436   /* stf_i.spill */,
  &gname220   /* sub */,
  &gname220   /* sub_1 */,
  &gname220   /* sub_i */,
  &gname499   /* sum */,
  &gname643   /* sxt1 */,
  &gname643   /* sxt2 */,
  &gname643   /* sxt4 */,
  &gname562   /* sync.i */,
  &gname517   /* tak */,
  &gname571   /* tbit.z */,
  &gname571   /* tbit.z.unc */,
  &gname571   /* tbit.z.and */,
  &gname571   /* tbit.z.or */,
  &gname571   /* tbit.z.or.andcm */,
  &gname571   /* tbit.nz.and */,
  &gname571   /* tbit.nz.or */,
  &gname571   /* tbit.nz.or.andcm */,
  &gname535   /* thash */,
  &gname652   /* tnat.z */,
  &gname652   /* tnat.z.unc */,
  &gname652   /* tnat.z.and */,
  &gname652   /* tnat.z.or */,
  &gname652   /* tnat.z.or.andcm */,
  &gname652   /* tnat.nz.and */,
  &gname652   /* tnat.nz.or */,
  &gname652   /* tnat.nz.or.andcm */,
  &gname517   /* tpa */,
  &gname535   /* ttag */,
  &gname310   /* unpack1.l */,
  &gname310   /* unpack1.h */,
  &gname310   /* unpack2.l */,
  &gname310   /* unpack2.h */,
  &gname310   /* unpack4.l */,
  &gname310   /* unpack4.h */,
  &gname391   /* xchg1 */,
  &gname391   /* xchg2 */,
  &gname391   /* xchg4 */,
  &gname391   /* xchg8 */,
  &gname634   /* xma.l */,
  &gname634   /* xma.h */,
  &gname634   /* xma.hu */,
  &gname238   /* xor */,
  &gname238   /* xor_i */,
  &gname643   /* zxt1 */,
  &gname643   /* zxt2 */,
  &gname643   /* zxt4 */,
  &gname481   /* alloc */,
  &gname22    /* br */,
  &gname22    /* br_r */,
  &gname22    /* brl */,
  &gname229   /* cmp.eq.orcm */,
  &gname229   /* cmp.eq.andcm */,
  &gname229   /* cmp.eq.and.orcm */,
  &gname229   /* cmp.ne */,
  &gname229   /* cmp.ne.unc */,
  &gname229   /* cmp.ne.orcm */,
  &gname229   /* cmp.ne.andcm */,
  &gname229   /* cmp.ne.and.orcm */,
  &gname229   /* cmp.le */,
  &gname229   /* cmp.le.unc */,
  &gname229   /* cmp.gt */,
  &gname229   /* cmp.gt.unc */,
  &gname229   /* cmp.ge */,
  &gname229   /* cmp.ge.unc */,
  &gname229   /* cmp.leu */,
  &gname229   /* cmp.leu.unc */,
  &gname229   /* cmp.gtu */,
  &gname229   /* cmp.gtu.unc */,
  &gname229   /* cmp.geu */,
  &gname229   /* cmp.geu.unc */,
  &gname229   /* cmp_z1.lt.orcm */,
  &gname229   /* cmp_z1.lt.andcm */,
  &gname229   /* cmp_z1.lt.and.orcm */,
  &gname229   /* cmp_z1.le.orcm */,
  &gname229   /* cmp_z1.le.andcm */,
  &gname229   /* cmp_z1.le.and.orcm */,
  &gname229   /* cmp_z1.gt.orcm */,
  &gname229   /* cmp_z1.gt.andcm */,
  &gname229   /* cmp_z1.gt.and.orcm */,
  &gname229   /* cmp_z1.ge.orcm */,
  &gname229   /* cmp_z1.ge.andcm */,
  &gname229   /* cmp_z1.ge.and.orcm */,
  &gname229   /* cmp_z2.lt.orcm */,
  &gname229   /* cmp_z2.lt.andcm */,
  &gname229   /* cmp_z2.lt.and.orcm */,
  &gname229   /* cmp_z2.le.orcm */,
  &gname229   /* cmp_z2.le.andcm */,
  &gname229   /* cmp_z2.le.and.orcm */,
  &gname229   /* cmp_z2.gt.orcm */,
  &gname229   /* cmp_z2.gt.andcm */,
  &gname229   /* cmp_z2.gt.and.orcm */,
  &gname229   /* cmp_z2.ge.orcm */,
  &gname229   /* cmp_z2.ge.andcm */,
  &gname229   /* cmp_z2.ge.and.orcm */,
  &gname229   /* cmp_z2.lt.and */,
  &gname229   /* cmp_z2.lt.or */,
  &gname229   /* cmp_z2.lt.or.andcm */,
  &gname229   /* cmp_z2.le.and */,
  &gname229   /* cmp_z2.le.or */,
  &gname229   /* cmp_z2.le.or.andcm */,
  &gname229   /* cmp_z2.gt.and */,
  &gname229   /* cmp_z2.gt.or */,
  &gname229   /* cmp_z2.gt.or.andcm */,
  &gname229   /* cmp_z2.ge.and */,
  &gname229   /* cmp_z2.ge.or */,
  &gname229   /* cmp_z2.ge.or.andcm */,
  &gname229   /* cmp_i.eq.orcm */,
  &gname229   /* cmp_i.eq.andcm */,
  &gname229   /* cmp_i.eq.and.orcm */,
  &gname229   /* cmp_i.ne */,
  &gname229   /* cmp_i.ne.unc */,
  &gname229   /* cmp_i.ne.orcm */,
  &gname229   /* cmp_i.ne.andcm */,
  &gname229   /* cmp_i.ne.and.orcm */,
  &gname229   /* cmp_i.le */,
  &gname229   /* cmp_i.le.unc */,
  &gname229   /* cmp_i.gt */,
  &gname229   /* cmp_i.gt.unc */,
  &gname229   /* cmp_i.ge */,
  &gname229   /* cmp_i.ge.unc */,
  &gname229   /* cmp_i.leu */,
  &gname229   /* cmp_i.leu.unc */,
  &gname229   /* cmp_i.gtu */,
  &gname229   /* cmp_i.gtu.unc */,
  &gname229   /* cmp_i.geu */,
  &gname229   /* cmp_i.geu.unc */,
  &gname229   /* cmp4.eq.orcm */,
  &gname229   /* cmp4.eq.andcm */,
  &gname229   /* cmp4.eq.and.orcm */,
  &gname229   /* cmp4.ne */,
  &gname229   /* cmp4.ne.unc */,
  &gname229   /* cmp4.ne.orcm */,
  &gname229   /* cmp4.ne.andcm */,
  &gname229   /* cmp4.ne.and.orcm */,
  &gname229   /* cmp4.le */,
  &gname229   /* cmp4.le.unc */,
  &gname229   /* cmp4.gt */,
  &gname229   /* cmp4.gt.unc */,
  &gname229   /* cmp4.ge */,
  &gname229   /* cmp4.ge.unc */,
  &gname229   /* cmp4.leu */,
  &gname229   /* cmp4.leu.unc */,
  &gname229   /* cmp4.gtu */,
  &gname229   /* cmp4.gtu.unc */,
  &gname229   /* cmp4.geu */,
  &gname229   /* cmp4.geu.unc */,
  &gname229   /* cmp4_z1.lt.orcm */,
  &gname229   /* cmp4_z1.lt.andcm */,
  &gname229   /* cmp4_z1.lt.and.orcm */,
  &gname229   /* cmp4_z1.le.orcm */,
  &gname229   /* cmp4_z1.le.andcm */,
  &gname229   /* cmp4_z1.le.and.orcm */,
  &gname229   /* cmp4_z1.gt.orcm */,
  &gname229   /* cmp4_z1.gt.andcm */,
  &gname229   /* cmp4_z1.gt.and.orcm */,
  &gname229   /* cmp4_z1.ge.orcm */,
  &gname229   /* cmp4_z1.ge.andcm */,
  &gname229   /* cmp4_z1.ge.and.orcm */,
  &gname229   /* cmp4_z2.lt.orcm */,
  &gname229   /* cmp4_z2.lt.andcm */,
  &gname229   /* cmp4_z2.lt.and.orcm */,
  &gname229   /* cmp4_z2.le.orcm */,
  &gname229   /* cmp4_z2.le.andcm */,
  &gname229   /* cmp4_z2.le.and.orcm */,
  &gname229   /* cmp4_z2.gt.orcm */,
  &gname229   /* cmp4_z2.gt.andcm */,
  &gname229   /* cmp4_z2.gt.and.orcm */,
  &gname229   /* cmp4_z2.ge.orcm */,
  &gname229   /* cmp4_z2.ge.andcm */,
  &gname229   /* cmp4_z2.ge.and.orcm */,
  &gname229   /* cmp4_z2.lt.and */,
  &gname229   /* cmp4_z2.lt.or */,
  &gname229   /* cmp4_z2.lt.or.andcm */,
  &gname229   /* cmp4_z2.le.and */,
  &gname229   /* cmp4_z2.le.or */,
  &gname229   /* cmp4_z2.le.or.andcm */,
  &gname229   /* cmp4_z2.gt.and */,
  &gname229   /* cmp4_z2.gt.or */,
  &gname229   /* cmp4_z2.gt.or.andcm */,
  &gname229   /* cmp4_z2.ge.and */,
  &gname229   /* cmp4_z2.ge.or */,
  &gname229   /* cmp4_z2.ge.or.andcm */,
  &gname229   /* cmp4_i.eq.orcm */,
  &gname229   /* cmp4_i.eq.andcm */,
  &gname229   /* cmp4_i.eq.and.orcm */,
  &gname229   /* cmp4_i.ne */,
  &gname229   /* cmp4_i.ne.unc */,
  &gname229   /* cmp4_i.ne.orcm */,
  &gname229   /* cmp4_i.ne.andcm */,
  &gname229   /* cmp4_i.ne.and.orcm */,
  &gname229   /* cmp4_i.le */,
  &gname229   /* cmp4_i.le.unc */,
  &gname229   /* cmp4_i.gt */,
  &gname229   /* cmp4_i.gt.unc */,
  &gname229   /* cmp4_i.ge */,
  &gname229   /* cmp4_i.ge.unc */,
  &gname229   /* cmp4_i.leu */,
  &gname229   /* cmp4_i.leu.unc */,
  &gname229   /* cmp4_i.gtu */,
  &gname229   /* cmp4_i.gtu.unc */,
  &gname229   /* cmp4_i.geu */,
  &gname229   /* cmp4_i.geu.unc */,
  &gname139   /* fabs */,
  &gname130   /* fadd */,
  &gname130   /* fadd.s */,
  &gname130   /* fadd.d */,
  &gname94    /* fclass.nm */,
  &gname94    /* fclass.nm.unc */,
  &gname94    /* fcmp.gt */,
  &gname94    /* fcmp.gt.unc */,
  &gname94    /* fcmp.ge */,
  &gname94    /* fcmp.ge.unc */,
  &gname94    /* fcmp.neq */,
  &gname94    /* fcmp.neq.unc */,
  &gname94    /* fcmp.nlt */,
  &gname94    /* fcmp.nlt.unc */,
  &gname94    /* fcmp.nle */,
  &gname94    /* fcmp.nle.unc */,
  &gname94    /* fcmp.ngt */,
  &gname94    /* fcmp.ngt.unc */,
  &gname94    /* fcmp.nge */,
  &gname94    /* fcmp.nge.unc */,
  &gname94    /* fcmp.ord */,
  &gname94    /* fcmp.ord.unc */,
  &gname130   /* fcvt.xuf */,
  &gname130   /* fcvt.xuf.s */,
  &gname130   /* fcvt.xuf.d */,
  &gname130   /* fmpy */,
  &gname130   /* fmpy.s */,
  &gname130   /* fmpy.d */,
  &gname139   /* fneg */,
  &gname139   /* fnegabs */,
  &gname130   /* fnmpy */,
  &gname130   /* fnmpy.s */,
  &gname130   /* fnmpy.d */,
  &gname130   /* fnorm */,
  &gname130   /* fnorm.s */,
  &gname130   /* fnorm.d */,
  &gname427   /* fpabs */,
  &gname427   /* fpcmp.gt */,
  &gname427   /* fpcmp.ge */,
  &gname427   /* fpcmp.ngt */,
  &gname427   /* fpcmp.nge */,
  &gname409   /* fpmpy */,
  &gname427   /* fpneg */,
  &gname427   /* fpnegabs */,
  &gname409   /* fpnmpy */,
  &gname130   /* fsub */,
  &gname130   /* fsub.s */,
  &gname130   /* fsub.d */,
  &gname598   /* mov_t_br */,
  &gname139   /* mov_f */,
  &gname220   /* mov */,
  &gname220   /* mov_i */,
  &gname247   /* shl_i */,
  &gname247   /* shr_i */,
  &gname247   /* shr_i.u */,
  &gname571   /* tbit.nz */,
  &gname571   /* tbit.nz.unc */,
  &gname652   /* tnat.nz */,
  &gname652   /* tnat.nz.unc */,
  &gname634   /* xma.lu */,
  &gname634   /* xmpy.l */,
  &gname634   /* xmpy.lu */,
  &gname634   /* xmpy.h */,
  &gname634   /* xmpy.hu */,
  &gname661   /* break */,
  &gname661   /* chk.s */,
  &gname229   /* cmp.lt.and */,
  &gname229   /* cmp.lt.or */,
  &gname229   /* cmp.lt.or.andcm */,
  &gname229   /* cmp.le.and */,
  &gname229   /* cmp.le.or */,
  &gname229   /* cmp.le.or.andcm */,
  &gname229   /* cmp.gt.and */,
  &gname229   /* cmp.gt.or */,
  &gname229   /* cmp.gt.or.andcm */,
  &gname229   /* cmp.ge.and */,
  &gname229   /* cmp.ge.or */,
  &gname229   /* cmp.ge.or.andcm */,
  &gname229   /* cmp.lt.orcm */,
  &gname229   /* cmp.lt.andcm */,
  &gname229   /* cmp.lt.and.orcm */,
  &gname229   /* cmp.le.orcm */,
  &gname229   /* cmp.le.andcm */,
  &gname229   /* cmp.le.and.orcm */,
  &gname229   /* cmp.gt.orcm */,
  &gname229   /* cmp.gt.andcm */,
  &gname229   /* cmp.gt.and.orcm */,
  &gname229   /* cmp.ge.orcm */,
  &gname229   /* cmp.ge.andcm */,
  &gname229   /* cmp.ge.and.orcm */,
  &gname229   /* cmp4.lt.and */,
  &gname229   /* cmp4.lt.or */,
  &gname229   /* cmp4.lt.or.andcm */,
  &gname229   /* cmp4.le.and */,
  &gname229   /* cmp4.le.or */,
  &gname229   /* cmp4.le.or.andcm */,
  &gname229   /* cmp4.gt.and */,
  &gname229   /* cmp4.gt.or */,
  &gname229   /* cmp4.gt.or.andcm */,
  &gname229   /* cmp4.ge.and */,
  &gname229   /* cmp4.ge.or */,
  &gname229   /* cmp4.ge.or.andcm */,
  &gname229   /* cmp4.lt.orcm */,
  &gname229   /* cmp4.lt.andcm */,
  &gname229   /* cmp4.lt.and.orcm */,
  &gname229   /* cmp4.le.orcm */,
  &gname229   /* cmp4.le.andcm */,
  &gname229   /* cmp4.le.and.orcm */,
  &gname229   /* cmp4.gt.orcm */,
  &gname229   /* cmp4.gt.andcm */,
  &gname229   /* cmp4.gt.and.orcm */,
  &gname229   /* cmp4.ge.orcm */,
  &gname229   /* cmp4.ge.andcm */,
  &gname229   /* cmp4.ge.and.orcm */,
  &gname661   /* mov_f_ar */,
  &gname661   /* mov_t_ar_r */,
  &gname661   /* mov_t_ar_i */,
  &gname661   /* nop */,
  &gname661   /* asm */,
  &gname661   /* intrncall */,
  &gname661   /* spadjust */,
  &gname661   /* copy.br */,
  &gname0     /* begin.pregtn */,
  &gname0     /* end.pregtn */,
  &gname0     /* bwd.bar */,
  &gname0     /* fwd.bar */,
  &gname0     /* dfixup */,
  &gname0     /* ffixup */,
  &gname0     /* ifixup */,
  &gname0     /* label */,
  &gname0     /* noop */
};
