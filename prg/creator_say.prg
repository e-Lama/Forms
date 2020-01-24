#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_say INHERIT Creator

EXPORTED:

    METHOD edit_form()

ENDCLASS LOCK

METHOD edit_form() CLASS Creator_say

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorSayHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorSayFooter'))
    LOCAL nTopLimit := IF(WSelect() == 0, Window():get_top(), 0)
    LOCAL nLeftLimit := IF(WSelect() == 0, Window():get_left(), 0)
    LOCAL nBottomLimit := IF(WSelect() == 0, Window():get_bottom(), MaxRow())
    LOCAL nRightLimit := IF(WSelect() == 0, Window():get_right(), MaxCol())
    LOCAL lFinish := .F.
    LOCAL lSave 
    LOCAL cScreen
    LOCAL nKey

    Window():refresh_header()
    Window():refresh_footer()

    SAVE SCREEN TO cScreen

    ::set_type(OBJECT_SAY)
    ::make_form_array()

    DO WHILE !lFinish

        RESTORE SCREEN FROM cScreen

        ::display_form()
        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_UP
                IF ::get_value(N_ROW_SAY) - 1 >= nTopLimit
                    ::decrement(N_ROW_SAY)
                ENDIF
            CASE nKey == K_LEFT
                IF ::get_value(N_COL_SAY) - 1 >= nLeftLimit
                    ::decrement(N_COL_SAY)
                ENDIF
            CASE nKey == K_DOWN
                IF ::get_value(N_ROW_SAY) + 1 <= nBottomLimit
                    ::increment(N_ROW_SAY)
                ENDIF
            CASE nKey == K_RIGHT
                IF ::get_value(N_COL_SAY) + 1 <= nRightLimit
                    ::increment(N_COL_SAY)
                ENDIF
            CASE nKey == K_ENTER
                ::form_fast_edit(cScreen)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(GETLIST)
                ENDIF
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave'))
                        IF ::save_form()
                            lFinish := .T.
                            lSave := .T.
                        ELSE
                            lFinish := .F.
                            lSave := .F.
                        ENDIF
                    ELSE 
                        lFinish := .T.
                        lSave := .F.
                    ENDIF
                ENDIF
        ENDCASE
    ENDDO

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    Window():refresh_header()
    Window():refresh_footer()

RETURN lSave
