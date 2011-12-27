{application, micex_mtesrl,
   [
      {description, "MICEX MTESRL line handler"},
      {vsn, "0.1"},
      {modules, [micex_mtesrl, micex_mtesrl_sup, micex_mtesrl_srv]},
      {registered, [micex_mtesrl_sup, micex_mtesrl_srv]},
      {application, [kernel, stdlib]},
      {mod, {micex_mtesrl, []}},
      {start_phases, []}
   ]
}.
