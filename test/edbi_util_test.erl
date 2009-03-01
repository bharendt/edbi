
-include_lib("include/eunit.hrl").


get_option_test() ->
  Options = [{option1, "Value1"}, {option2, "Value2"}, {option3, value3}],
  ?assertMatch({value, "Value1"}, get_option(option1, Options)),
  ?assertMatch({value, "Value2"}, get_option(option2, Options)),
  ?assertMatch({value, value3}, get_option(option3, Options)),
  ?assertMatch(false, get_option(option4, Options)).

get_option_with_default_value_test() ->
  Options = [{option1, "Value1"}, {option2, "Value2"}, {option3, value3}],
  ?assertMatch({value, "Value1"}, get_option(option1, Options, "DefaultValue")),
  ?assertMatch({value, "Value2"}, get_option(option2, Options, "DefaultValue")),
  ?assertMatch({value, value3}, get_option(option3, Options, "DefaultValue")),
  ?assertMatch({value, "DefaultValue"},  get_option(option4, Options, "DefaultValue")).
  
parse_url_params_test() ->
  ?assertMatch([{reconnect, true}], parse_url_params("reconnect")),
  ?assertMatch([{reconnect, false}], parse_url_params("reconnect=false")),
  ?assertMatch([{timeout, 1000}], parse_url_params("timeout=1000")),
  ?assertMatch([{float_value, 0.33}], parse_url_params("float_value=0.33")),
  ?assertMatch([{user, "username"}], parse_url_params("user=username")),

  ?assertMatch([{user, "username"}, {reconnect, true}, {timeout, 1000}], 
                parse_url_params("user=username&reconnect&timeout=1000")),
  ?assertMatch([{user, "username"}, {timeout, 1000}, {reconnect, true}], 
                parse_url_params("user=username&timeout=1000&reconnect")),
  ?assertMatch([{reconnect, true}, {user, "username"}, {timeout, 1000}], 
                parse_url_params("reconnect&user=username&timeout=1000")).
  
parse_url_test() ->
  ?assertMatch({mysql, "localhost", 3306, "products", [{user, "user_name"}, {password, "secret"}]}, 
               parse_url("mysql://localhost:3306/products?user=user_name&password=secret")),
  ?assertMatch({mysql, "192.168.0.1", default, "products", [{user, "user_name"}]}, 
               parse_url("mysql://192.168.0.1/products?user=user_name")),
  ?assertMatch({postgres, "db.myhost.com", default, "products", []}, 
               parse_url("postgres://db.myhost.com/products")),
  ?assertMatch({postgres, "db.myhost.com", 5432, "products", []}, 
               parse_url("postgres://db.myhost.com:5432/products")),
  ok.  
  
  
dummy_test() ->
  ok.
  