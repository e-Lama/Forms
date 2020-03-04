#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_window INHERIT Creator

EXPORTED:

    METHOD edit_form(xFormCode, xGetPos)

ENDCLASS LOCK

METHOD edit_form(xFormCode, xGetPos) CLASS Creator_window

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorWindowHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorWindowFooter'))
    LOCAL lActiveUpperLeftCorner := .T.
    LOCAL lFinish := .F.
    LOCAL lSave := .F.
    LOCAL nOldWindow
    LOCAL xFormCodeWithoutWindow
    LOCAL cScreen
    LOCAL nKey

    IF PCount() != 0
        WClose()
        CLEAR GETS
    ENDIF

    Window():refresh_header_footer()

    SAVE SCREEN TO cScreen

    ::clear_window_flag()
    ::set_type(OBJECT_WINDOW)

    IF PCount() != 0
        xFormCodeWithoutWindow := hb_ADel(AClone(xFormCode), 1, .T.)

        IF !prepare_form(xFormCodeWithoutWindow)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::make_form_array(xFormCode)
        ENDIF
    ELSE
        ::make_form_array()
    ENDIF

    DO WHILE !lFinish

        nOldWindow := WSelect()
        WSelect(0)
        RESTORE SCREEN FROM cScreen 
        WSelect(nOldWindow)

        ::display_form()

        IF PCount() != 0

            CLEAR GETS

            IF !prepare_form(xFormCodeWithoutWindow)
                Inform(Parser():log(''))
            ENDIF

        ENDIF

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_ALT_A
                lActiveUpperLeftCorner := !lActiveUpperLeftCorner
            CASE nKey == K_UP
                IF lActiveUpperLeftCorner 
                    IF ::get_value(N_TOP_WN) - 1 <= ::get_value(N_BOTTOM_WN) .AND. ::get_value(N_TOP_WN) >= Window():get_top()
                        ::decrement(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_WN) <= ::get_value(N_BOTTOM_WN) - 1 .AND. ::get_value(N_BOTTOM_WN) >= Window():get_top()
                        ::decrement(N_BOTTOM_WN)
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_WN) - 1 <= ::get_value(N_RIGHT_WN) .AND. ::get_value(N_LEFT_WN) >= Window():get_left()
                        ::decrement(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_WN) <= ::get_value(N_RIGHT_WN) - 1 .AND. ::get_value(N_RIGHT_WN) >= Window():get_left()
                        ::decrement(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_TOP_WN) + 1 <= ::get_value(N_BOTTOM_WN) .AND. ::get_value(N_TOP_WN) <= Window():get_bottom()
                        ::increment(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_WN) <= ::get_value(N_BOTTOM_WN) + 1 .AND. ::get_value(N_BOTTOM_WN) <= Window():get_bottom()
                        ::increment(N_BOTTOM_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_WN) + 1 <= ::get_value(N_RIGHT_WN) .AND. ::get_value(N_LEFT_WN) <= Window():get_right()
                        ::increment(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_WN) <= ::get_value(N_RIGHT_WN) + 1 .AND. ::get_value(N_RIGHT_WN) <= Window():get_right()
                        ::increment(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::form_fast_edit(WRow(), WCol(), WLastRow(), WLastCol(), cScreen, xFormCodeWithoutWindow, xGetPos)
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
    Window():refresh_header_footer()

RETURN lSave
