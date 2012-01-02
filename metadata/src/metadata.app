{application, metadata,
   [
      {description, "metadata"},
      {vsn, "0.1"},
      {modules, [metadata, metadata_app, metadata_sup, metadata_srv]},
      {registered, [metadata_sup, 'qinfo.metadata']},
      {application, [kernel, stdlib]},
      {mod, {metadata_app, []}},
      {start_phases, []}
   ]
}.
