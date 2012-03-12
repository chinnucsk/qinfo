-type commodity() :: string().
-type type() :: 'equity' | 'future' | 'bond' | 'itf' | 'spot'.
-type exchange() :: string().
-type exch_symbol() :: string(). % name of instrument on exchange
-type hour() :: pos_integer().
-type min()  :: pos_integer().
-type time_interval() :: {{hour(), min()}, {hour(), min()}}.
-type day_of_week() :: 'mon' | 'tue' | 'wed' | 'thu' | 'fri' | 'sat' | 'sun'.
-type sched_of_day() :: {day_of_week(), 'enabled' | 'disabled', string()}. % E.g. {mon, enabled, "09:30-15:30"}
-type sched_of_day_fmt() :: {pos_integer(), 'enabled' | 'disabled', [time_interval()]}.

-record(instrument_key, {exch_symbol  :: exch_symbol(),
                         type         :: type,
                         exchange     :: exchange}).

-record(commodity_key, {commodity     :: commodity(),
                        type          :: type(),
                        exchange      :: exchange()}).

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
                         expiration   :: 'undef' | calendar:datetime(),
                         ref          :: pos_integer()}).

%-record(new_service, {service  :: {'global' | 'local', atom()},
%                      settings :: list(),
%                      schedule :: sched_of_day()}).

-record(trade, {price :: float(),
                qty   :: pos_integer()}).

-record(setting, {name, value, description, validator = undef}).

-record(commodity, {key             :: #commodity_key{},
                    class_code      :: string(),
                    enabled = false :: boolean(),
                    alias = undef   :: 'undef' | string()}).

-record(instrument, {key                :: #instrument_key{},
                     commodity          :: #commodity_key{},
                     full_name          :: string(),
                     expiration = undef :: 'undef' | calendar:datetime(),
                     limit_up           :: float(),
                     limit_down         :: float(),
                     lot_size           :: pos_integer(),
                     updated            :: calendar:datetime(),
                     ref                :: pos_integer()}).

-record(service, {service            :: tuple(),
                  description        :: string(),
                  settings = []      :: list(),
                  schedule = []      :: [sched_of_day()],
                  fmt_schedule = []  :: [sched_of_day_fmt()]}).

-record(exchange, {name            :: string(),
                   description = "" :: string()}).
