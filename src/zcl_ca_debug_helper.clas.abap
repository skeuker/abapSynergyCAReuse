class ZCL_CA_DEBUG_HELPER definition
  public
  create public .

public section.

  class-methods HANDLE_DEBUGGING
    importing
      !ID_CODE_POINT type ZCA_CODE_POINT .
  class-methods HANDLE_DEBUGGING_Q
    importing
      !ID_CODE_POINT type ZCA_CODE_POINT
    exceptions
      ERROR .
  class-methods HANDLE_DEBUGGING_T
    importing
      !ID_CODE_POINT type ZCA_CODE_POINT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_DEBUG_HELPER IMPLEMENTATION.


METHOD handle_debugging.

* check for background
  CHECK sy-batch = abap_true.

* check if we need to debug (wait for capture)
  SELECT COUNT( * )
    FROM zca_debug                                       "#EC CI_BYPASS
    INTO @DATA(ld_count)
    WHERE codepoint = @id_code_point
    AND   uname     = @sy-uname
    AND   datum     = @sy-datum
    AND   from_time <= @sy-uzeit
    AND   to_time   >= @sy-uzeit
    AND   active     = @abap_true.

* if it's setup to wait, then wait, a max of 60 seconds
  IF ld_count > 0.
    DO 60 TIMES.
      DO 10 TIMES.
        SELECT SINGLE @abap_true
          FROM zca_debug
          INTO @DATA(ld_exists).
        IF sy-subrc = 0.
          "
        ELSE.
          "
        ENDIF.
      ENDDO.
      WAIT UP TO 1 SECONDS.
    ENDDO.
  ENDIF.

ENDMETHOD.


METHOD HANDLE_DEBUGGING_Q.

* check if we need to debug (wait for capture)
  SELECT COUNT( * )
    FROM zca_debug                                       "#EC CI_BYPASS
    INTO @DATA(ld_count)
    WHERE codepoint = @id_code_point
    AND   uname     = @sy-uname
    AND   datum     = @sy-datum
    AND   from_time <= @sy-uzeit
    AND   to_time   >= @sy-uzeit
    AND   active     = @abap_true.

* if it's setup to wait, then wait, a max of 60 seconds
  IF ld_count > 0.
    MESSAGE 'Error for queue debugging' TYPE 'E' RAISING error.
  ENDIF.

ENDMETHOD.


method handle_debugging_t.

* local declarations
  data: ld_1 type timestampl,
        ld_2 type timestampl,
        ld_3 type timestampl.

* check if we need to debug (wait for capture)
  select count( * )
    from zca_debug                                       "#EC CI_BYPASS
    into @data(ld_count)
    where codepoint = @id_code_point
    and   uname     = @sy-uname
    and   datum     = @sy-datum
    and   from_time <= @sy-uzeit
    and   to_time   >= @sy-uzeit
    and   active     = @abap_true.

*...if it's setup to wait, then wait, a max of 80 seconds
  if ld_count > 0.
    get time stamp field ld_1.
    do.
      get time stamp field ld_2.
      cl_abap_tstmp=>subtract(
        exporting
          tstmp1 = ld_2
          tstmp2 = ld_1
        receiving
          r_secs = ld_3
      ).
      if ld_3 > 80.
        exit.
      endif.
    enddo.
  endif.

endmethod.
ENDCLASS.
