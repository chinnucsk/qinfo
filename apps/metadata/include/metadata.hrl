-type commodity() :: string().
-type type() :: 'equity' | 'future' | 'bond' | 'itf' | 'spot'.
-type exchange() :: string().
-type exch_name() :: string(). % name of instrument on exchange
-type hour() :: pos_integer().
-type min()  :: pos_integer().
-type time_interval() :: {{hour(), min()}, {hour(), min()}}.
-type commodity_key() :: {commodity(), type(), exchange()}.
-type instrument_key() :: {exch_name(), type(), exchange()}.
-type day_of_week() :: 'mon' | 'tue' | 'wed' | 'thu' | 'fri' | 'sat' | 'sun'.
-type sched_of_day() :: {day_of_week(), 'enabled' | 'disabled', string()}. % E.g. {mon, enabled, "09:30-15:30"}
-type sched_of_day_fmt() :: {pos_integer(), 'enabled' | 'disabled', [time_interval()]}.

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

-record(trade, {price :: float(),
                qty   :: pos_integer()}).

-record(setting, {name, value, description, validator = undef}).

-record(service, {service  :: {'global' | 'local', atom()},
                  settings :: list(),
                  schedule :: sched_of_day()}).

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

-record(m_service, {service            :: tuple(),
                    description        :: string(),
                    settings = []      :: list(),
                    schedule = []      :: [sched_of_day()],
                    fmt_schedule = []  :: [sched_of_day_fmt()]}).

-record(m_exchange, {name            :: string(),
                    description = "" :: string()}).
