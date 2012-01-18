-record(new_instrument, {name, exch, class_code, full_name, type, commodity, limit_up, limit_down, lot_size, expiration,
   ref}).
-record(get_instrument, {name, exch, class_code, full_name, type, commodity, limit_up, limit_down, lot_size, expiration,
   ref, qinfo_name}).
-record(trade, {price, qty}).
-record(service, {service, settings = [], schedule = []}).
-record(setting, {name, value, description, validator = undef}).

-record(m_instrument, {name, full_name, exch, expiration = undef, commodity, limit_up, limit_down,
      lot_size, type, enabled, ref}).
-record(m_commodity, {key, enabled = false, alias = undef}).
-record(m_service, {service, description, settings = [], schedule = []}).
