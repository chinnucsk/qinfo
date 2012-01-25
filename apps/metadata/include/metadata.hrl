-type commodity() :: string().
-type type() :: 'equity' | 'future' | 'bond' | 'itf' | 'spot'.
-type exchange() :: string().
-type exch_name() :: string(). % name of instrument on exchange
-type commodity_key() :: {commodity(), type(), exchange()}.
-type instrument_key() :: {exch_name(), type(), exchange()}.

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

-record(m_commodity, {key             :: commodity_key(),
                      class_code      :: string(),
                      enabled = false :: boolean(),
                      alias = undef   :: 'undef' | string()}).

-record(m_instrument, {key                :: instrument_key(),
                       commodity          :: commodity_key(),
                       full_name          :: string(),
                       expiration = undef :: 'undef' | calender:datetime(),
                       limit_up           :: float(),
                       limit_down         :: float(),
                       lot_size           :: pos_integer(),
                       ref                :: pos_integer()}).

-record(m_service, {service       :: tuple(),
                    description   :: string(),
                    enabled       :: boolean(),
                    settings = [] :: list(),
                    schedule = [] :: list()}).

-record(m_exchange, {name            :: string(),
                    description = "" :: string()}).
