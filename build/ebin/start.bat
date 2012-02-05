set ERL_LIBS=%CD%

werl -name qinfo@192.168.1.185 ^
     -boot start_sasl ^
     -metadata inets_port 8000 ^
     -config start.config ^
     -setcookie qinfo ^
     -eval "application:start(nprocreg)" ^
     -eval "application:start(metadata)"
