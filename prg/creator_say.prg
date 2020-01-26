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
    LOCAL nOldWindow := WSelect()
    LOCAL nTopLimit := IF(WSelect() == 0, Window():get_top(), 0)
    LOCAL nLeftLimit := IF(WSelect() == 0, Window():get_left(), 0)
    LOCAL nBottomLimit := IF(WSelect() == 0, Window():get_bottom(), MaxRow())
    LOCAL nRightLimit := IF(WSelect() == 0, Window():get_right(), MaxCol())
    LOCAL lFinish := .F.
    LOCAL nTop := WRow()
    LOCAL nLeft := WCol()
    LOCAL nBottom := WLastRow()
    LOCAL nRight := WLastCol()
    LOCAL lSave := .F.
    LOCAL cScreen
    LOCAL nKey

    IF WSelect() > 0
        WSelect(0)
        cScreen := SaveScreen(nTop, nLeft, nBottom, nRight)
        WSelect(nOldWindow)

        Window():refresh_header()
        Window():refresh_footer()

        WClose()
    ELSE
        Window():refresh_header()
        Window():refresh_footer()

        CLEAR GETS

        IF !prepare_form()
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            SAVE SCREEN TO cScreen
        ENDIF
    ENDIF

    IF !lFinish
        ::set_type(OBJECT_SAY)
        CLEAR GETS

        IF !prepare_form()
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::make_form_array()
        ENDIF
    ENDIF

    DO WHILE !lFinish

        IF WSelect() > 0
            WSelect(0)
            RestScreen(nTop, nLeft, nBottom, nRight, cScreen)
            WSelect(nOldWindow)
        ELSE
            RESTORE SCREEN FROM cScreen
        ENDIF

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
                ::form_fast_edit(nTop, nLeft, nBottom, nRight, cScreen)
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
                        ENDIF
                    ELSE 
                        lFinish := .T.
                    ENDIF
                ENDIF
        ENDCASE
    ENDDO

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    Window():refresh_header()
    Window():refresh_footer()

RETURN lSave
