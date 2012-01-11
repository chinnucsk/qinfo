{application, rts_plaza2,
   [
      {description, "RTS Plaza2 line handler"},
      {vsn, "0.1"},
      {modules, [rts_plaza2_app, rts_plaza2_sup, rts_plaza2_srv]},
      {registered, [rts_plaza2_sup, rts_plaza2_srv]},
      {application, [kernel, stdlib]},
      {mod, {rts_plaza2_app, []}},
      {start_phases, []}
   ]
}.
