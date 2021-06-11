MODULE utils
    IMPLICIT NONE
    PUBLIC :: IsVowel
    CONTAINS
    FUNCTION IsVowel(input_char) RESULT(is_vowel)
        CHARACTER(LEN=1), INTENT (IN) :: input_char
        LOGICAL :: is_vowel

        SELECT CASE (input_char)
            CASE ('a','e','i','o','u', 'A','E','I','O','U')
                is_vowel = .TRUE.
            CASE DEFAULT
                is_vowel = .FALSE.
        END SELECT

        RETURN
    END FUNCTION IsVowel

END MODULE utils

PROGRAM p5
    USE utils
    IMPLICIT NONE
    INTEGER, PARAMETER :: max_word_size = 100
    CHARACTER(LEN = max_word_size) :: word
    INTEGER :: i, vowel_num = 0

    OPEN (UNIT = 1, FILE = "prog5_word.txt", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
    READ (UNIT = 1, FMT=*) word

    DO i=1, max_word_size
        IF (IsVowel(word(i:i))) THEN
            vowel_num = vowel_num + 1
        END IF
    END DO

    PRINT *, "Word len = ", len_trim(word)
    PRINT *, "Vowels count = ", vowel_num

END PROGRAM p5
