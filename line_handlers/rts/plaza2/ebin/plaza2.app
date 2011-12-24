{application, plaza2,
   [
      {description, "Plaza2 line handler"},
      {vsn, "0.1"},
      {modules, [plaza2, plaza2_sup, plaza2_srv]},
      {registered, [plaza2_sup, plaza2_srv]},
      {application, [kernel, stdlib]},
      {mod, {plaza2, []}},
      {start_phases, []}
   ]
}.
