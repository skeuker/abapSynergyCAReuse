"! <p class="shorttext synchronized" lang="en">COM Data Replication Framework (DRF) helper class</p>
class ZCL_CA_DRF_HELPER definition
  public
  final
  create public .

public section.

  constants GC_PARAM_MSG_QUALIF type TEXT20 value 'ZCA_MSG_QUALIFIER' ##NO_TEXT.
  constants GC_PARAM_FILE_PREFIX type TEXT20 value 'ZCA_FILE_PREFIX' ##NO_TEXT.
  constants GC_PARAM_COL_HEADINGS type TEXT20 value 'ZCA_COLUMN_HEADINGS' ##NO_TEXT.
  constants GC_PARAM_PACK_SIZE type TEXT20 value 'PACK_SIZE_BULK' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
  methods CONSTRUCTOR
    importing
      !IO_DRF_OUT type ref to ZCA_IF_DRF_OUTBOUND_EXTEND
    raising
      CX_DRF_OUTBOUND .
    "! <p class="shorttext synchronized" lang="en">Get parameters of last run</p>
  methods GET_LAST_RUNPARAMS
    exporting
      !ES_LASTRUN_PARAMS type DRF_S_RUNTIME_PARAM_NO_REFS
    raising
      CX_DRF_OUTBOUND .
    "! <p class="shorttext synchronized" lang="en">Get change period</p>
  methods GET_CHANGE_PERIOD
    exporting
      !ES_CHANGE_PERIOD type ZCA_S_DRF_CHANGE_PERIOD
    raising
      CX_DRF_OUTBOUND .
    "! <p class="shorttext synchronized" lang="en">Set SOAP message header in proxy message</p>
  methods SET_SOAP_MESSAGEHEADER
    changing
      !CS_PROXY_OUTPUT type ANY
    raising
      CX_DRF_OUTBOUND .
  methods SET_MESSAGEHEADER
    importing
      !IR_DRF_OUTB type ref to ZCA_IF_DRF_OUTBOUND_EXTEND optional
    changing
      !CS_MESSAGE_HEADER type ZCA_S_DRF_MESSAGE_HEADER
    raising
      CX_DRF_OUTBOUND .
  methods GET_FILE_NAME
    importing
      !ID_FILE_EXT type STRING default '.txt'
    returning
      value(RD_FILENAME) type STRING
    raising
      CX_DRF_OUTBOUND .
  methods GET_FILE_NAME_WITH_DETAIL
    importing
      !ID_FILETYPE type STRING default '.txt'
      !ID_INBETWEEN type STRING optional
    returning
      value(RD_FILENAME) type STRING
    raising
      CX_DRF_OUTBOUND .
protected section.

  data MO_DRF_OUT type ref to ZCA_IF_DRF_OUTBOUND_EXTEND .
  data MD_TOTALMSG_COUNT type NUMC5 .
  data MD_MESSAGE_COUNTER type NUMC5 .
private section.
ENDCLASS.



CLASS ZCL_CA_DRF_HELPER IMPLEMENTATION.


  method constructor.

*For exception handling
    try.

*Cast to extension interface
        mo_drf_out ?= io_drf_out.

*Exception handling: failed to cast.
      catch cx_sy_move_cast_error.

*...message handling: to use the helper implement the extension interface
        message e001(zca_drf) with mo_drf_out->ms_runtime_params-outb_impl into data(ld_message).
        mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).

*...raise DRF processing exception
        raise exception type cx_drf_outbound
          exporting
            application             = mo_drf_out->ms_runtime_params-appl
            outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
            business_system         = mo_drf_out->ms_runtime_params-business_system.

    endtry.

  endmethod.


  method get_change_period.

*Local data declaration
    data: ls_lastrun_params  type drf_s_runtime_param_no_refs,
          lt_unfiltered_objs type zsd_s_drf_replkey_customer,
          ls_change_period   type zca_s_drf_change_period.

*For exception handling
    try.

*Get requested change period from current run parameters
        if mo_drf_out->ms_runtime_params-time_interval_supplied eq abap_true.

*...fill lower change period boundary
          ls_change_period-date_from = mo_drf_out->ms_runtime_params-crt_date_lower_limit.
          ls_change_period-time_from = mo_drf_out->ms_runtime_params-crt_time_lower_limit.

*...fill upper change period boundary
          ls_change_period-date_to = mo_drf_out->ms_runtime_params-crt_date_upper_limit.
          ls_change_period-time_to = mo_drf_out->ms_runtime_params-crt_time_upper_limit.

*...provide default upper date boundary where applicable
          if ls_change_period-date_to is initial.
            ls_change_period-date_to = '99991231'.
          endif.

*...provide default upper time boundary where applicable
          if ls_change_period-time_to is initial.
            ls_change_period-time_to = '235959'.
          endif.

        endif.

*Derive change period from last run where applicable
        if ls_change_period is initial.

*...get parameters of last replication run
          get_last_runparams( importing es_lastrun_params = ls_lastrun_params ).

*...fill lower change period boundary
          ls_change_period-date_from = ls_lastrun_params-start_date.
          ls_change_period-time_from = ls_lastrun_params-start_time.

*...fill upper change period horizon
          ls_change_period-date_to = '99991231'.
          ls_change_period-time_to = '235959'.

        endif.

*Exception handling: failed to get change period
      catch cx_drf_outbound into data(lx_drf_outbound).
        raise exception lx_drf_outbound.

    endtry.

*Feedback to caller
    es_change_period = ls_change_period.

  endmethod.


  method get_file_name.

*Local declarations
    data: ld_file_fullname  type string.

*Get file prefix from DRF parameters
    try.
        data(ld_file_prefix) = mo_drf_out->ms_runtime_params-outb_parameter[ outb_parameter = gc_param_file_prefix ]-value.
      catch cx_sy_itab_line_not_found.
        message e002(zpp_drf) with gc_param_file_prefix into data(ld_dummy).
        call method mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
        raise exception type cx_drf_outbound.
    endtry.

*Get logical path
    data(ld_logical_path) = mo_drf_out->ms_runtime_params-file_path.

    if ld_logical_path is initial.
      message e001(zpp_drf) into ld_dummy.
      call method mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
      raise exception type cx_drf_outbound.
    endif.

*Construct file name
    convert date sy-datum time sy-uzeit into time stamp data(ld_timestamp) time zone sy-zonlo.
    data(ld_file_name) = |{ ld_file_prefix }| && |{ ld_timestamp }| && |{ id_file_ext }|.

*Get physical path from Logical path
    call function 'FILE_GET_NAME_USING_PATH'
      exporting
        logical_path               = ld_logical_path
        file_name                  = ld_file_name
        eleminate_blanks           = 'X'
      importing
        file_name_with_path        = ld_file_fullname
      exceptions
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        others                     = 5.

    if sy-subrc <> 0.
      call method mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
      raise exception type cx_drf_outbound.
    endif.

*Return to caller
    rd_filename = ld_file_fullname.

  endmethod.


METHOD get_file_name_with_detail.

* local declarations
  DATA: ld_file_fullname  TYPE string.

* get file prefix from drf parameters
  TRY.
      DATA(ld_file_prefix) = mo_drf_out->ms_runtime_params-outb_parameter[ outb_parameter = gc_param_file_prefix ]-value.
    CATCH cx_sy_itab_line_not_found.
      MESSAGE e002(zpp_drf) WITH gc_param_file_prefix INTO DATA(ld_dummy).
      CALL METHOD mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
      RAISE EXCEPTION TYPE cx_drf_outbound.
  ENDTRY.

* get logical path
  DATA(ld_logical_path) = mo_drf_out->ms_runtime_params-file_path.

  IF ld_logical_path IS INITIAL.
    MESSAGE e001(zpp_drf) INTO ld_dummy.
    CALL METHOD mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
    RAISE EXCEPTION TYPE cx_drf_outbound.
  ENDIF.

* construct file name
  CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP DATA(ld_timestamp) TIME ZONE sy-zonlo.
  IF id_inbetween IS INITIAL.
    DATA(ld_file_name) = |{ ld_file_prefix }| && |{ ld_timestamp }| && |{ id_filetype }|.
  ELSE.
    ld_file_name = |{ ld_file_prefix }| && |{ id_inbetween } | && |_| && |{ ld_timestamp }| && |{ id_filetype }|.
  ENDIF.

* get physical path from logical path
  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      logical_path               = ld_logical_path
      file_name                  = ld_file_name
      eleminate_blanks           = 'X'
    IMPORTING
      file_name_with_path        = ld_file_fullname
    EXCEPTIONS
      path_not_found             = 1
      missing_parameter          = 2
      operating_system_not_found = 3
      file_system_not_found      = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    CALL METHOD mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
    RAISE EXCEPTION TYPE cx_drf_outbound.
  ENDIF.

* return to caller
  rd_filename = ld_file_fullname.

ENDMETHOD.


  method get_last_runparams.

*Local data declaration
    data: ls_lastrun_params  type drf_s_runtime_param_no_refs,
          lt_unfiltered_objs type zsd_s_drf_replkey_customer,
          lt_drf_replication type drf_to_replication,
          ld_tstamp_lastrun  type timestamp.

*Get previous replication runs for this business system and outbound implementation
    cl_drf_access_replication=>repl_mode_read_multi(
     exporting
        iv_appl             =    mo_drf_out->ms_runtime_params-appl
        iv_business_system  =    mo_drf_out->ms_runtime_params-business_system
        iv_outb_impl        =    mo_drf_out->ms_runtime_params-outb_impl
      importing
        ev_not_found        =    data(ld_not_found)
        et_drfd_replication =    lt_drf_replication ).

*Get latest change run timestamp
    select single max( timestamp ) as timestamp
      from @lt_drf_replication as dummy
      where replication_mode = 'C' "Change
        and runmod eq '' group by outbound_impl
      into @data(ld_lastrun_timestamp).                   "#EC CI_SUBRC

*Get last initialization timestamp where applicable
    if ld_lastrun_timestamp is initial.

*...get latest initialization run timestamp
      select single max( timestamp ) as timestamp
        from @lt_drf_replication as dummy
        where replication_mode = 'I' "Initialization
          and runmod eq '' group by outbound_impl
        into @ld_lastrun_timestamp.                       "#EC CI_SUBRC

    endif.

*...exception handling: no previous initialization or change run exists
    if ld_lastrun_timestamp is initial.
      message e001(zsd_drf) with mo_drf_out->ms_runtime_params-outb_impl
         into data(ld_message).
      mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
      raise exception type cx_drf_outbound
        exporting
          application             = mo_drf_out->ms_runtime_params-appl
          outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
          business_system         = mo_drf_out->ms_runtime_params-business_system.
    endif.

*Get lastest run attributes
    data(ls_lastrun) = lt_drf_replication[ timestamp = ld_lastrun_timestamp ]. "#EC CI_SORTSEQ

*Get runtime parameters of last change mode run
    cl_drf_servout_log=>get_00_runtime_param(
      exporting
        iv_appl                  =    ls_lastrun-appl
        iv_run_id                =    ls_lastrun-run_id
        iv_outb_impl             =    ls_lastrun-outbound_impl
      importing
        es_runtime_param_no_refs = ls_lastrun_params ).

*Feedback to caller
    es_lastrun_params = ls_lastrun_params.

  endmethod.


  method set_messageheader.

*Local data declaration
    data: ls_message_header type zca_s_drf_message_header,
          lt_outb_parameter type drf_to_outb_param,
          ld_own_logsys     type logsys.

*For exception handling
    try.

*Get access to the 'MessageHeader' data
        assign cs_message_header to field-symbol(<ls_proxyroot_output>).
        if sy-subrc eq 0.
          assign <ls_proxyroot_output> to field-symbol(<ls_message_header>).
        endif.

*Exception handling: couldn't get access to 'MessageHeader' element in proxy root
        if <ls_message_header> is not assigned.
          message e005(zsd_drf) into data(ld_message).
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Get own logical system to set as sender of this message
        call function 'OWN_LOGICAL_SYSTEM_GET'
          importing
            own_logical_system = ld_own_logsys
          exceptions
            others             = 0.

*Exception handling: couldn't determine own logical system
        if ld_own_logsys is initial.
          message e006(zsd_drf) with sy-mandt sy-sysid into ld_message.
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Set message header attributes: receiver
        ls_message_header-sender-content = ld_own_logsys.
        ls_message_header-sender-agency = sy-sysid.
        ls_message_header-sender-schema = 'LogicalSystem'.

*Set message header attributes: receiver
        ls_message_header-receiver-content = mo_drf_out->ms_runtime_params-business_system.
        ls_message_header-receiver-agency = 'DRF'.
        ls_message_header-receiver-schema = 'BussinessSystem'.

*Set message header attributes: message id
        ls_message_header-id-content = mo_drf_out->ms_runtime_params-run_id.
        shift ls_message_header-id-content left deleting leading '0'.
        ls_message_header-id-agency = 'DRF'.
        ls_message_header-id-schema = 'RunID'.

*Get DRF outbound implementation specific attributes
        if ir_drf_outb is bound.

*Set message header attributes: message qualifier
          cl_drf_access_cust_data=>select_outb_par_for_impl_appl(
           exporting
             iv_appl           = ir_drf_outb->ms_runtime_params-appl
             iv_outb_impl      = ir_drf_outb->ms_runtime_params-outb_impl
           importing
             et_outb_parameter = lt_outb_parameter ).

*...get message qualifier outbound implementation parameter
          assign lt_outb_parameter[ outb_parameter = gc_param_msg_qualif ] to field-symbol(<ld_qualifier>).

*...adopt message qualifier parameter where available
          if <ld_qualifier> is assigned.
            ls_message_header-id-content = <ld_qualifier>.
            ls_message_header-id-agency = 'DRF'.
            ls_message_header-id-schema = 'Parameter'.
          endif.

        endif.

*Adopt message header attributes into message
        <ls_message_header> = corresponding #( ls_message_header ).

*Exception handling: failed to set SOAP message header
      catch cx_drf_outbound into data(lx_drf_outbound).
        raise exception lx_drf_outbound.

    endtry.

  endmethod.


  method set_soap_messageheader.

*Local data declaration
    data: lo_proxyoutput_descr type ref to cl_abap_structdescr,
          lo_proxyroot_descr   type ref to cl_abap_structdescr,
          lt_comps_proxyoutput type abap_component_tab,
          ls_comps_proxyoutput type abap_componentdescr,
          lt_comps_proxyroot   type abap_component_tab,
          ls_comps_proxyroot   type abap_componentdescr,
          ls_message_header    type zca_message_header,
          lt_outb_parameter    type drf_to_outb_param,
          ld_own_logsys        type logsys.

*For exception handling
    try.

*Get proxy structure description
        lo_proxyoutput_descr ?= cl_abap_typedescr=>describe_by_data( p_data = cs_proxy_output ).

*Get components of the proxy structure
        lt_comps_proxyoutput = lo_proxyoutput_descr->get_components( ).

*Remove proxy controller element
        delete lt_comps_proxyoutput where name = 'CONTROLLER'.

*Exception handling: more than root element in proxy output
        if lines( lt_comps_proxyoutput ) ne 1.
          message e002(zsd_drf) with mo_drf_out->ms_runtime_params-outb_impl
             into data(ld_message).
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Get proxy root element
        assign lt_comps_proxyoutput[ 1 ] to field-symbol(<ls_comp_proxyroot>).

*Exception handling: proxy root type information not available
        if <ls_comp_proxyroot>-type is not bound.
          message e003(zsd_drf) into ld_message.
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Get components of the proxy root structure
        lo_proxyroot_descr ?= <ls_comp_proxyroot>-type.
        lt_comps_proxyroot = lo_proxyroot_descr->get_components( ).

*Get 'MessageHeader' in proxy root structure
        read table lt_comps_proxyroot with key name = 'MESSAGE_HEADER' assigning field-symbol(<ls_comp_messageheader>).

*Exception handling: proxy root does not have a 'MessageHeader' element
        if sy-subrc ne 0.
          message e004(zsd_drf) with mo_drf_out->ms_runtime_params-outb_impl into ld_message.
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Get access to the 'MessageHeader' data
        assign component <ls_comp_proxyroot>-name of structure cs_proxy_output to field-symbol(<ls_proxyroot_output>).
        if sy-subrc eq 0.
          assign component <ls_comp_messageheader>-name of structure <ls_proxyroot_output> to field-symbol(<ls_message_header>).
        endif.

*Exception handling: couldn't get access to 'MessageHeader' element in proxy root
        if <ls_message_header> is not assigned.
          message e005(zsd_drf) into ld_message.
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Get own logical system to set as sender of this message
        call function 'OWN_LOGICAL_SYSTEM_GET'
          importing
            own_logical_system = ld_own_logsys
          exceptions
            others             = 0.

*Exception handling: couldn't determine own logical system
        if ld_own_logsys is initial.
          message e006(zsd_drf) with sy-mandt sy-sysid into ld_message.
          mo_drf_out->ms_runtime_params-bal->add_msg_sy( ).
          raise exception type cx_drf_outbound
            exporting
              application             = mo_drf_out->ms_runtime_params-appl
              outbound_implementation = mo_drf_out->ms_runtime_params-outb_impl
              business_system         = mo_drf_out->ms_runtime_params-business_system.
        endif.

*Set message header attributes: receiver
        ls_message_header-sender-content = ld_own_logsys.
        ls_message_header-sender-agency = sy-sysid.
        ls_message_header-sender-schema = 'LogicalSystem'.

*Set message header attributes: receiver
        ls_message_header-receiver-content = mo_drf_out->ms_runtime_params-business_system.
        ls_message_header-receiver-agency = 'DRF'.
        ls_message_header-receiver-schema = 'BusinessSystem'.

*Set message header attributes: message id
        ls_message_header-id-content = mo_drf_out->ms_runtime_params-run_id.
        shift ls_message_header-id-content left deleting leading '0'.
        ls_message_header-id-agency = 'DRF'.
        ls_message_header-id-schema = 'RunID'.

*Get DRF outbound implementation specific attributes
        if mo_drf_out is bound.

*...get outbound implementation parameters
          cl_drf_access_cust_data=>select_outb_par_for_impl_appl(
           exporting
             iv_appl           = mo_drf_out->ms_runtime_params-appl
             iv_outb_impl      = mo_drf_out->ms_runtime_params-outb_impl
           importing
             et_outb_parameter = lt_outb_parameter ).

*...get message qualifier outbound implementation parameter
          assign lt_outb_parameter[ outb_parameter = gc_param_msg_qualif ] to field-symbol(<ld_qualifier>).

*...adopt message qualifier parameter where available
          if <ld_qualifier> is assigned.
            ls_message_header-qualifier-content = <ld_qualifier>-value.
            ls_message_header-qualifier-agency = 'DRF'.
            ls_message_header-qualifier-schema = 'Parameter'.
          endif.

*...set transmission attributes where applicable
          if mo_drf_out->md_rootentity_count is not initial.

*....compute total message count in this transmission where applicable
            if md_totalmsg_count is initial.

*.....get pack size outbound implementation parameter
              assign lt_outb_parameter[ outb_parameter = gc_param_pack_size ] to field-symbol(<ld_packsize>).

*.....compute total count of messages
              if <ld_packsize> is assigned.
                md_totalmsg_count = ceil( mo_drf_out->md_rootentity_count / <ld_packsize>-value ).
              else.
                clear md_message_counter.
                md_totalmsg_count = 1.
              endif.

            endif.

*....increment message counter
            add 1 to md_message_counter.

*....set transmission attributes
            ls_message_header-transmission-total_message_count = | { md_totalmsg_count alpha = out } |.
            ls_message_header-transmission-message_counter = | { md_message_counter alpha = out }  |.
            condense ls_message_header-transmission-total_message_count.
            condense ls_message_header-transmission-message_counter.

          endif.

        endif.

*Adopt message header attributes into message
        <ls_message_header> = corresponding #( ls_message_header ).

*Exception handling: failed to set SOAP message header
      catch cx_drf_outbound into data(lx_drf_outbound).
        raise exception lx_drf_outbound.

    endtry.

  endmethod.
ENDCLASS.
