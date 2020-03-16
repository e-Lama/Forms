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

    ::_clear_window_flag()
    ::_set_type(OBJECT_WINDOW)

    IF PCount() != 0
        xFormCodeWithoutWindow := hb_ADel(AClone(xFormCode), 1, .T.)

        IF !prepare_form(xFormCodeWithoutWindow)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::_make_form_array(xFormCode)
        ENDIF
    ELSE
        ::_make_form_array()
    ENDIF

    DO WHILE !lFinish

        nOldWindow := WSelect()
        WSelect(0)
        RESTORE SCREEN FROM cScreen 
        WSelect(nOldWindow)

        ::_display_form()

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
                    IF ::_get_value(N_TOP_WN) - 1 <= ::_get_value(N_BOTTOM_WN) .AND. ::_get_value(N_TOP_WN) >= Window():get_top()
                        ::_decrement(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_TOP_WN) <= ::_get_value(N_BOTTOM_WN) - 1 .AND. ::_get_value(N_BOTTOM_WN) >= Window():get_top()
                        ::_decrement(N_BOTTOM_WN)
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_LEFT_WN) - 1 <= ::_get_value(N_RIGHT_WN) .AND. ::_get_value(N_LEFT_WN) >= Window():get_left()
                        ::_decrement(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_LEFT_WN) <= ::_get_value(N_RIGHT_WN) - 1 .AND. ::_get_value(N_RIGHT_WN) >= Window():get_left()
                        ::_decrement(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_TOP_WN) + 1 <= ::_get_value(N_BOTTOM_WN) .AND. ::_get_value(N_TOP_WN) <= Window():get_bottom()
                        ::_increment(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_TOP_WN) <= ::_get_value(N_BOTTOM_WN) + 1 .AND. ::_get_value(N_BOTTOM_WN) <= Window():get_bottom()
                        ::_increment(N_BOTTOM_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_LEFT_WN) + 1 <= ::_get_value(N_RIGHT_WN) .AND. ::_get_value(N_LEFT_WN) <= Window():get_right()
                        ::_increment(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_LEFT_WN) <= ::_get_value(N_RIGHT_WN) + 1 .AND. ::_get_value(N_RIGHT_WN) <= Window():get_right()
                        ::_increment(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::_form_fast_edit(WRow(), WCol(), WLastRow(), WLastCol(), cScreen, xFormCodeWithoutWindow, xGetPos)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(GETLIST)
                ENDIF
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave'))
                        IF ::_save_form()
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
