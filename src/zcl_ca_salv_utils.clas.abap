class ZCL_CA_SALV_UTILS definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    field_cataglogue_db_table_type type standard table of ltdxdata with default key .
  types:
    begin of ty_fields_color,
        field type lvc_fname,
        color type lvc_s_colo,
      end of ty_fields_color .
  types:
    tty_fields_color type standard table of ty_fields_color .
  types:
    begin of ty_fields_tech,
        fieldname type lvc_fname,
      end of ty_fields_tech .
  types:
    tty_fields_tech type standard table of ty_fields_tech .

  class-data SO_GRID type ref to CL_GUI_ALV_GRID .
  class-data SO_SALV_TABLE type ref to CL_SALV_TABLE .

  class-methods CALL_EASY_SALV
    importing
      !ID_REPID type SY-REPID optional
      !ID_TEXT_USE_TECH type BOOLE_D default ABAP_FALSE
      !ID_TITLE type STRING optional
    changing
      !CT_TABLE type DATA .
  class-methods HIDE_COLUMNS_EMPTY
    importing
      !IO_SALV type ref to CL_SALV_TABLE
      !IT_TABLE type TABLE .
  class-methods CALL_EASY_SALV_OWN_PFSTATUS
    importing
      !IO_FUNCTION_SET_HANDLER_CLASS type ref to OBJECT
      !ID_FUNCTION_SET_HANDLER_METHOD type STRING
      !ID_REPID type SY-REPID optional
      !ID_PFSTATUS_REPORT type SYREPID
      !ID_PFSTATUS_NAME type SYPFKEY
      !ID_HOTSPOT_FIELD type LVC_FNAME optional
    changing
      !CT_TABLE type DATA .
  class-methods REFRESH .
  class-methods REFRESH_FIELDS_TECH .
  class-methods REFRESH_FIELDS_COLOR .
  class-methods ADD_FIELDS_COLOR
    importing
      !ID_FIELD type LVC_FNAME
      !IS_COLOR type LVC_S_COLO .
  class-methods ADD_FIELDS_TECHNICAL
    importing
      !ID_FIELD type LVC_FNAME .
  class-methods CALL_EASY_SALV_AS_POPUP
    importing
      !ID_REPID type SY-REPID optional
      !ID_START_COLUMN type I
      !ID_START_ROW type I
      !ID_END_COLUMN type I
      !ID_END_ROW type I
    changing
      !CT_TABLE type DATA .
  class-methods GET_FIELDS_FOR_TABLE_DATA
    importing
      !IT_TABLE type TABLE
    returning
      value(RT_FIELDS) type STRINGTAB .
  protected section.

    class-data st_fields_color type tty_fields_color .
    class-data st_fields_tech type tty_fields_tech .
*"* private components of class ZCL_HELPER_ALV
*"* do not include other source files here!!!
  private section.

    class-methods call_easy_salv_handle_color
      importing
        !io_salv_columns type ref to cl_salv_columns .
    class-methods call_easy_salv_handle_tech
      importing
        !io_salv_columns type ref to cl_salv_columns .
    class-methods call_easy_salv_handle_text_f
      importing
        !io_salv_columns type ref to cl_salv_columns .

ENDCLASS.



CLASS ZCL_CA_SALV_UTILS IMPLEMENTATION.


  method add_fields_color.

* local data
    data ls_fields_color type ty_fields_color.

* set fields to structure
    ls_fields_color-field = id_field.
    ls_fields_color-color = is_color.

* add to table
    append ls_fields_color to st_fields_color.

  endmethod.


  method add_fields_technical.

* add to table
    append id_field to st_fields_tech.

* make sure there is no duplicates
    sort st_fields_tech.
    delete adjacent duplicates from st_fields_tech.

  endmethod.


  method call_easy_salv.

* local data
    data: lr_table     type ref to cl_salv_table,
          lr_functions type ref to cl_salv_functions_list,
          lr_cols      type ref to cl_salv_columns,
          lr_layout    type ref to cl_salv_layout,
          ls_key       type salv_s_layout_key.
    data lo_column               type ref to cl_salv_column_table.

    include <icon>.

* create the ALV
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = lr_table
          changing
            t_table      = ct_table ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

* activate ALV generic Functions
    lr_functions = lr_table->get_functions( ).
    lr_functions->set_all( 'X' ).

* set the Column optimization
    lr_cols = lr_table->get_columns( ).
    lr_cols->set_optimize( 'X' ).

    try.
        lo_column ?= lr_cols->get_column( 'LIGHTS' ). " check if there is a column lights, that will be lights than
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
      catch cx_root.
    endtry.

    try.
        lo_column ?= lr_cols->get_column( 'ICON' ). " check if there is a column icon, that will be lights than
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
      catch cx_root.
    endtry.

* handle color and tech field setting
    zcl_ca_salv_utils=>call_easy_salv_handle_color( lr_cols ).
    zcl_ca_salv_utils=>call_easy_salv_handle_tech( lr_cols ).
    if id_text_use_tech eq abap_true.
      zcl_ca_salv_utils=>call_easy_salv_handle_text_f( lr_cols ).
    endif.

* check for saved layouts if report id is provided
    if id_repid is not initial.

*   care for layout
      ls_key-report = sy-repid.
      lr_layout = lr_table->get_layout( ).
      lr_layout->set_key( ls_key ).

*   set usage of default Layouts
      lr_layout->set_default( abap_true ).

*   set Layout save restriction
      lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    endif.

*   We will set this COLOR table field name of the internal table to
*   COLUMNS tab reference for the specific colors
    try.
        data: lo_cols_tab type ref to cl_salv_columns_table.

*     get Columns object
        lo_cols_tab = lr_table->get_columns( ).
        lo_cols_tab->set_color_column( 'COLOR_TAB' ).

      catch cx_salv_data_error.                         "#EC NO_HANDLER
    endtry.

* set the title if requested
    if id_title is not initial.
      data(lo_display) = lr_table->get_display_settings( ).
      lo_display->set_list_header( conv #( id_title ) ).
    endif.

* display the table
    lr_table->display( ).

  endmethod.


  method call_easy_salv_as_popup.

* local data
    data: lr_table     type ref to cl_salv_table,
          lr_functions type ref to cl_salv_functions_list,
          lr_cols      type ref to cl_salv_columns,
          lr_layout    type ref to cl_salv_layout,
          ls_key       type salv_s_layout_key.
    data lo_column               type ref to cl_salv_column_table.

    include <icon>.

* create the ALV
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = lr_table
          changing
            t_table      = ct_table ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

* popup?
    lr_table->set_screen_popup(
      start_column = id_start_column
      end_column   = id_end_column
      start_line   = id_start_row
      end_line     = id_end_row
      ).

* activate ALV generic Functions
    lr_functions = lr_table->get_functions( ).
    lr_functions->set_all( 'X' ).

* set the Column optimization
    lr_cols = lr_table->get_columns( ).
    lr_cols->set_optimize( 'X' ).

    try.
        lo_column ?= lr_cols->get_column( 'LIGHTS' ). " check if there is a column lights, that will be lights than
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
      catch cx_root.
    endtry.

    try.
        lo_column ?= lr_cols->get_column( 'ICON' ). " check if there is a column icon, that will be lights than
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
      catch cx_root.
    endtry.

* handle color and tech field setting
    zcl_ca_salv_utils=>call_easy_salv_handle_color( lr_cols ).
    zcl_ca_salv_utils=>call_easy_salv_handle_tech( lr_cols ).

* check for saved layouts if report id is provided
    if id_repid is not initial.

*   care for layout
      ls_key-report = sy-repid.
      lr_layout = lr_table->get_layout( ).
      lr_layout->set_key( ls_key ).

*   set usage of default Layouts
      lr_layout->set_default( abap_true ).

*   set Layout save restriction
      lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    endif.

*   We will set this COLOR table field name of the internal table to
*   COLUMNS tab reference for the specific colors
    try.
        data: lo_cols_tab type ref to cl_salv_columns_table.

*     get Columns object
        lo_cols_tab = lr_table->get_columns( ).
        lo_cols_tab->set_color_column( 'COLOR_TAB' ).

      catch cx_salv_data_error.                         "#EC NO_HANDLER
    endtry.

* display the table
    lr_table->display( ).

  endmethod.


  method call_easy_salv_handle_color.

* local data
    data lo_column type ref to cl_salv_column_table.

    field-symbols <ls_fields_color> like line of st_fields_color.

* set colors if pre-set
    loop at st_fields_color assigning <ls_fields_color>.

      try.
          lo_column ?= io_salv_columns->get_column( <ls_fields_color>-field ).
          lo_column->set_color( <ls_fields_color>-color ).
        catch cx_salv_not_found.                        "#EC NO_HANDLER
      endtry.

    endloop.

  endmethod.


  method call_easy_salv_handle_tech.

* local data
    data lo_column type ref to cl_salv_column_table.

    field-symbols <ls_fields_tech> like line of st_fields_tech.

* set colors if pre-set
    loop at st_fields_tech assigning <ls_fields_tech>.

      try.
          lo_column ?= io_salv_columns->get_column( <ls_fields_tech>-fieldname ).
          lo_column->set_technical( if_salv_c_bool_sap=>true ).
        catch cx_salv_not_found.                        "#EC NO_HANDLER
      endtry.

    endloop.

  endmethod.


  method call_easy_salv_handle_text_f.

* local data
    data lv_medium type scrtext_m.
    data lv_short type scrtext_s.
    data lt_columns type salv_t_column_ref.

    field-symbols <ls_columns> like line of lt_columns.

    lt_columns = io_salv_columns->get( ).

* set colors if pre-set
    loop at lt_columns assigning <ls_columns>.

      try.

          lv_short = lv_medium = <ls_columns>-r_column->get_columnname( ).

          <ls_columns>-r_column->set_long_text( <ls_columns>-r_column->get_columnname( ) ).
          <ls_columns>-r_column->set_medium_text( lv_medium ).
          <ls_columns>-r_column->set_short_text( lv_short ).

        catch cx_salv_not_found.                        "#EC NO_HANDLER
      endtry.

    endloop.

  endmethod.


  method call_easy_salv_own_pfstatus.

* local data
    data: lr_functions type ref to cl_salv_functions_list,
          lo_columns   type ref to cl_salv_columns,
          lr_layout    type ref to cl_salv_layout,
          ls_key       type salv_s_layout_key.
    data lo_column               type ref to cl_salv_column_table.
    data: lr_selections type ref to cl_salv_selections.

    data: l_text type string,
          l_icon type string.

    field-symbols <ls_fields_color> like line of st_fields_color.
    include <icon>.

* create the ALV
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = so_salv_table
          changing
            t_table      = ct_table ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

* set PF Status to the save one specially created
    so_salv_table->set_screen_status(
      pfstatus      = id_pfstatus_name
      report        = id_pfstatus_report
      set_functions = so_salv_table->c_functions_all ).

* set the Column optimization
    lo_columns = so_salv_table->get_columns( ).
    lo_columns->set_optimize( 'X' ).

    try.
        lo_column ?= lo_columns->get_column( 'LIGHTS' ). " check if there is a column lights, that will be lights than
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
      catch cx_root.
    endtry.

    try.
        lo_column ?= lo_columns->get_column( 'ICON' ). " check if there is a column icon, that will be lights than
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
      catch cx_root.
    endtry.

* handle color and tech field setting
    zcl_ca_salv_utils=>call_easy_salv_handle_color( lo_columns ).
    zcl_ca_salv_utils=>call_easy_salv_handle_tech( lo_columns ).


* check for saved layouts if report id is provided
    if id_repid is not initial.

*   care for layout
      ls_key-report = sy-repid.
      lr_layout = so_salv_table->get_layout( ).
      lr_layout->set_key( ls_key ).

*   set usage of default Layouts
      lr_layout->set_default( abap_true ).

*   set Layout save restriction
      lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    endif.

* set event handler
    data: lr_events type ref to cl_salv_events_table.

    lr_events = so_salv_table->get_event( ).
    call method io_function_set_handler_class->(id_function_set_handler_method) exporting io_events = lr_events.

*   We will set this COLOR table field name of the internal table to
*   COLUMNS tab reference for the specific colors
    try.
        data: lo_columns_tab type ref to cl_salv_columns_table.

*     get Columns object
        lo_columns_tab = so_salv_table->get_columns( ).
        lo_columns_tab->set_color_column( 'COLOR_TAB' ).

      catch cx_salv_data_error.                         "#EC NO_HANDLER
    endtry.

    try.
        if id_hotspot_field is not initial.
          lo_column ?= lo_columns_tab->get_column( id_hotspot_field ).
          lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        endif.
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

* get selection class
    lr_selections = so_salv_table->get_selections( ).

* set selection mode
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* display the table
    so_salv_table->display( ).

  endmethod.


  method get_fields_for_table_data.

* local data
    data lo_typedescr type ref to cl_abap_typedescr.
    data lo_tabledescr type ref to cl_abap_tabledescr.
    data lo_structdescr type ref to cl_abap_structdescr.
    data lv_component_name type abap_compname.

    field-symbols <ls_components> type abap_compdescr.

* describe by name
    lo_typedescr = cl_abap_tabledescr=>describe_by_data( it_table ).

* check type descr there
    if lo_typedescr is initial.
      return.
    endif.

    try.
*     try downcast to table now and then get structure of it
        lo_tabledescr ?= lo_typedescr.
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
      catch cx_root.
*     if error, return cause it is not a structure than
        return.
    endtry.

* prepare output
    loop at lo_structdescr->components assigning <ls_components>.
      lv_component_name = <ls_components>-name.
      append lv_component_name to rt_fields.
    endloop.

  endmethod.


  method hide_columns_empty.

* local data
    data lt_fields  type stringtab.
    data lv_where type string.
    data lv_columnname type lvc_fname.
    data lo_columns type ref to cl_salv_columns_table.
    data lo_column  type ref to cl_salv_column.

    field-symbols <ls_line> type any.
    field-symbols <lv_field> type string.

* get columns
    lo_columns = io_salv->get_columns( ).

* get structure definition
    lt_fields = get_fields_for_table_data( it_table ).

* process each field of the structure
    loop at lt_fields assigning <lv_field>.

*   re-init
      clear lv_where.

*   build where condition for the field
      concatenate `'` <lv_field> `' IS NOT INITIAL`  into lv_where.

*   check in the table for existance of values
      loop at it_table assigning <ls_line> where (lv_where).
        exit.
      endloop.
*  if sy-subrc eq 4 then the loop wasnt processed, which means all the items are space in the result, we can hide it
      if sy-subrc eq 4.

*     set column name to be hidden
        lv_columnname = <lv_field>.

*     hide fields
        try.
            lo_column = lo_columns->get_column( lv_columnname ).
            lo_column->set_visible( abap_false ).
          catch cx_root.
        endtry.
      endif.

    endloop.

  endmethod.


  method refresh.

    so_salv_table->refresh( ).
    cl_gui_cfw=>flush( ).

  endmethod.


  method refresh_fields_color.

    refresh st_fields_color.

  endmethod.


  method refresh_fields_tech.

    refresh st_fields_tech.

  endmethod.
ENDCLASS.
