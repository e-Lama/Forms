#include "parser.ch"

FUNCTION prepare_form()

    LOCAL hJson := hb_JsonDecode(dbVariables->json)

    IF ValType(hJson) != 'H'
        hJson := hb_Hash()
    ENDIF

RETURN IF(Empty(field->code), .T., Parser():prepare_form_from_record(hb_ATokens(field->code, OBJECT_SEPARATOR), hJson))

FUNCTION validate()

    LOCAL hJson := hb_JsonDecode(dbVariables->json)

    IF ValType(hJson) != 'H'
        hJson := hb_Hash()
    ENDIF

RETURN IF(Empty(field->code), .T., Parser():check_correctness(hb_ATokens(field->code, OBJECT_SEPARATOR), hJson))
