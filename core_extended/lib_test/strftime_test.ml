open Core.Std let _ = _squelch_unused_module_warning_
open Core_extended.Std

let () =
  let now = Time.now () in
  let crazy =
    Time.format now
      "\
      %%a:  %_25a\n\
      %%A:  %_25A\n\
      %%b:  %_25b\n\
      %%B:  %_25B\n\
      %%c:  %_25c\n\
      %%C:  %_25C\n\
      %%d:  %_25d\n\
      %%D:  %_25D\n\
      %%e:  %_25e\n\
      %%Ec: %_25Ec\n\
      %%EC: %_25EC\n\
      %%Ex: %_25Ex\n\
      %%EX: %_25EX\n\
      %%Ey: %_25Ey\n\
      %%EY: %_25EY\n\
      %%F:  %_25F\n\
      %%G:  %_25G\n\
      %%g:  %_25g\n\
      %%h:  %_25h\n\
      %%H:  %_25H\n\
      %%I:  %_25I\n\
      %%j:  %_25j\n\
      %%k:  %_25k\n\
      %%l:  %_25l\n\
      %%m:  %_25m\n\
      %%M:  %_25M\n\
      %%n:  %_25n\n\
      %%Od: %_25Od\n\
      %%Oe: %_25Oe\n\
      %%OH: %_25OH\n\
      %%OI: %_25OI\n\
      %%Om: %_25Om\n\
      %%OM: %_25OM\n\
      %%OS: %_25OS\n\
      %%Ou: %_25Ou\n\
      %%OU: %_25OU\n\
      %%OV: %_25OV\n\
      %%Ow: %_25Ow\n\
      %%OW: %_25OW\n\
      %%Oy: %_25Oy\n\
      %%p:  %_25p\n\
      %%P:  %_25P\n\
      %%r:  %_25r\n\
      %%R:  %_25R\n\
      %%s:  %_25s\n\
      %%S:  %_25S\n\
      %%t:  %_25t\n\
      %%T:  %_25T\n\
      %%u:  %_25u\n\
      %%U:  %_25U\n\
      %%V:  %_25V\n\
      %%w:  %_25w\n\
      %%W:  %_25W\n\
      %%x:  %_25x\n\
      %%X:  %_25X\n\
      %%y:  %_25y\n\
      %%Y:  %_25Y\n\
      %%z:  %_25z\n\
      %%Z:  %_25Z\n\
      %%%%: %_25%"
  in
  print_endline crazy
