-type commodity() :: string().
-type type :: 'equity' | 'future' | 'bond' | 'itf' | 'standard'.
-type exchange() :: string().
-type commodity_key() :: {commodity(), type(), exchange()}.
-type instrument_key() :: {string(), type(), exchange()}.

-record(new_instrument, {name         :: string(),
                         exch_name    :: string(),
                         exchange     :: string(),
                         class_code   :: string(),
                         full_name    :: string(),
                         type         :: type(),
                         commodity    :: commodity(),
                         limit_up     :: float(),
                         limit_down   :: float(),
                         lot_size     :: pos_integer(),
                         expiration   :: 'undef' | calender:datetime(),
                         ref          :: pos_integer()}).

-record(trade, {price, qty}).
-record(service, {service, enabled, settings = [], schedule = []}).
-record(setting, {name, value, description, validator = undef}).

-record(m_instrument, {key                :: instrument_key(),
                       exch_name          :: string(),
                       full_name          :: string(),
                       exchange           :: exchange(),
                       expiration = undef :: 'undef' | calender:datetime(),
                       commodity          :: commodity(),
                       limit_up           :: float(),
                       limit_down         :: float(),
                       lot_size           :: pos_integer(),
                       type               :: type(),
                       ref                :: pos_integer()}).

-record(m_commodity, {key             :: commodity_key(),
                      class_code      :: string(),
                      enabled = false :: boolean(),
                      alias = undef   :: 'undef' | string()}).

-record(m_service, {service, description, enabled, settings = [], schedule = []}).
-record(m_exchange, {name, description = ""}).
