-record(new_instrument, {name, exch, class_code, full_name, type, commodity, limit_up, limit_down, lot_size, expiration,
   ref}).
-record(get_instrument, {name, exch, class_code, full_name, type, commodity, limit_up, limit_down, lot_size, expiration,
   ref, qinfo_name}).
-record(trade, {price, qty}).
-record(service, {service, settings = [], schedule = []}).
