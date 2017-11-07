-include_lib("fast_xml/include/fxml.hrl").




-record(xmpp_utils_jid, {username = <<>>
                        ,server = <<>>
                        ,resource = <<>>}).




-record(xmpp_utils_xml, {kind = <<>>
                        ,from = #xmpp_utils_jid{}
                        ,to = #xmpp_utils_jid{}
                        ,type = <<>>
                        ,id = <<>>
                        ,children = []
                        ,metadata = #{}}).