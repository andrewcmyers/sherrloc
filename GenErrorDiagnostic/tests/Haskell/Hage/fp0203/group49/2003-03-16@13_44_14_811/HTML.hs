module HTML where
data HTML

  = HTMLString String
  | HTMLTag   String
  | TagInTag [HTML]



tagToHTMLTag :: String -> String -> HTML
tagToHTMLTag = tag

tag :: String -> String
tag a = "<" ++ a ++ ">"

-- signature should be: String -> String
-- 10,1-12   10,17-40   10,27-40   10,37-40
