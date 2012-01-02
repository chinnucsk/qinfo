{application, plaza2,
   [
      {description, "Plaza2 line handler"},
      {vsn, "0.1"},
      {modules, [plaza2_app, plaza2_sup, plaza2_srv]},
      {registered, [plaza2_sup, plaza2_srv]},
      {application, [kernel, stdlib]},
      {mod, {plaza2_app, []}},
      {start_phases, []}
   ]
}.
