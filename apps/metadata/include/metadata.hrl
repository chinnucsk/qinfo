-record(new_instrument, {name, exchange, class_code, full_name, type, commodity, limit_up, limit_down, lot_size, expiration,
   ref}).
-record(trade, {price, qty}).
-record(service, {service, enabled, settings = [], schedule = []}).
-record(setting, {name, value, description, validator = undef}).

-record(m_instrument, {name, full_name, exchange, expiration = undef, commodity, limit_up, limit_down,
      lot_size, type, enabled, ref}).
-record(m_commodity, {key, enabled = false, alias = undef}).
-record(m_service, {service, description, enabled, settings = [], schedule = []}).
-record(m_exchange, {name, description = ""}).
