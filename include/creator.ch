#define OBJECT_SEPARATOR Chr(10) + Chr(10)
#define LINE_SEPARATOR Chr(10)

#define CONSTANT 'C'
#define VARIABLE 'V'

#define STIFFED_NONE 0
#define STIFFED_VERTICALLY 1
#define STIFFED_HORIZONTALLY 2

//Type of a form
#define OBJECT 1
#define OBJECT_WINDOW 'WN'
#define OBJECT_BOX 'BX'
#define OBJECT_SAY 'SS'
#define OBJECT_GET 'GS'
#define OBJECT_CHECKBOX 'GC'
#define OBJECT_LISTBOX 'GL'
#define OBJECT_RADIOGROUP 'GR'
#define OBJECT_PUSHBUTTON 'PB'

//WINDOW
#define N_TOP_WN 1
#define N_LEFT_WN 2
#define N_BOTTOM_WN 3
#define N_RIGHT_WN 4
#define C_BOX_WN 5
#define C_COLOR_WN 6
#define NC_SHADOW_WN 7

//BOX
#define N_TOP_BOX 1
#define N_LEFT_BOX 2
#define N_BOTTOM_BOX 3
#define N_RIGHT_BOX 4
#define C_BOX_BOX 5
#define C_COLOR_BOX 6

//SAY
#define N_ROW_SAY 1
#define N_COL_SAY 2
#define C_EXPRESSION_SAY 3
#define C_PICTURE_SAY 4
#define C_COLOR_SAY 5

//GET
#define N_ROW_GET 1
#define N_COL_GET 2
#define C_EXPRESSION_GET 3
#define C_SAY_PICTURE_GET 4
#define C_SAY_COLOR_GET 5
#define X_ID_VAR_GET 6
#define C_GET_PICTURE_GET 7
#define C_GET_COLOR_GET 8
#define C_CAPTION_GET 9
#define C_MESSAGE_GET 10
#define C_WHEN_FNC_GET 11
#define C_VALID_FNC_GET 12

//CHECKBOX
#define N_ROW_CHB 1
#define N_COL_CHB 2
#define L_ID_VAR_CHB 3
#define C_CAPTION_CHB 4
#define C_MESSAGE_CHB 5
#define C_WHEN_FNC_CHB 6
#define C_VALID_FNC_CHB 7
#define C_COLOR_CHB 8
#define C_FOCUS_FNC_CHB 9
#define C_STATE_CHB 10
#define C_STYLE_CHB 11

//LISTBOX
#define N_TOP_LSB 1
#define N_LEFT_LSB 2
#define N_BOTTOM_LSB 3
#define N_RIGHT_LSB 4
#define NC_ID_VAR_LSB 5
#define A_LIST_LSB 6
#define C_CAPTION_LSB 7
#define C_MESSAGE_LSB 8
#define C_WHEN_FNC_LSB 9
#define C_VALID_FNC_LSB 10
#define C_COLOR_LSB 11
#define C_FOCUS_FNC_LSB 12
#define C_STATE_FNC_LSB 13
#define L_DROPDOWN_LSB 14
#define L_SCROLLBAR_LSB 15

//RADIOGROUP
#define N_TOP_RGB 1
#define N_LEFT_RGB 2
#define N_BOTTOM_RGB 3
#define N_RIGHT_RGB 4
#define NC_ID_VAR_RGB 5
#define A_GROUP_RGB 6
#define C_CAPTION_RGB 7
#define C_MESSAGE_RGB 8
#define C_COLOR_RGB 9
#define C_FOCUS_FNC_RGB 10
#define C_WHEN_FNC_RGB 11
#define C_VALID_FNC_RGB 12

//RADIOBUTTON
#define N_ROW_RBT 1
#define N_COL_RBT 2
#define C_CAPTION_RBT 3
#define C_VALUE_RBT 4

//PUSHBUTTON
#define N_ROW_PSB 1
#define N_COL_PSB 2
#define L_ID_VAR_PSB 3
#define C_CAPTION_PSB 4
#define C_MESSAGE_PSB 5
#define C_WHEN_FNC_PSB 6
#define C_VALID_FNC_PSB 7
#define C_COLOR_PSB 8
#define C_FOCUS_FNC_PSB 9
#define C_STATE_PSB 10
#define C_STYLE_PSB 11
