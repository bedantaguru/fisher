

from selenium import webdriver  

driver = webdriver.Chrome(executable_path="C:/Users/Nil/AppData/Local/binman/binman_chromedriver/win32/87.0.4280.20/chromedriver.exe")  


driver.get("https://b-ok.asia/book/2883645/4d513b")

driver.find_element_by_css_selector(css_selector=".addDownloadedBook").click()


driver.close()


# webdriver.ChromeOptions opt = new webdriver.ChromeOptions();

optc = webdriver.ChromeOptions()

opt = webdriver.opera.options.Options()

# prefs = {"proxy_switcher.automatic_connection": True,
#          "proxy_switcher.enabled": True,
#          "proxy_switcher.ui_visible", True
#         }

opt.add_experimental_option("prefs", 
  {"proxy_switcher.automatic_connection": True,
  "pproxy_switcher.enabled": True,
  "proxy_switcher.ui_visible": True})

opt.add_experimental_option("prefs", 
    {"freedom":{
      "proxy_switcher":{
        "automatic_connection":True,
        "automatic_connection_update_applied":True,
        "enabled":True,
        "forbidden":False,
        "ui_visible":True}}})
        

opt.set_capability("accept.ssl.certs",True)

opt.add_experimental_option("prefs",
  {
    "download.default_directory": "C:\\Users\\Nil\\Downloads\\etc"
  })

optc.add_experimental_option("prefs",
  {
    "download.default_directory": "C:\\Users\\Nil\\Downloads\\etc"
  })

optc.set_capability("google.chrome.options", optc.to_capabilities())

# optc.

# local router or serving ip

driver = webdriver.Opera(executable_path="C:/Users/Nil/Downloads/operadriver_win64/operadriver.exe", options=opt)

driver.get("opera://settings/vpn")

# ele = driver.find_element_by_id("vpn-key")

driver.get("opera://about")



# {"freedom":{
#   "proxy_switcher":{
#     "automatic_connection":true,
#     "automatic_connection_update_applied":true,
#     "bytes_transferred":"0",
#     "enabled":true,
#     "forbidden":false,
#     "last_ui_interaction_time":1606768493.430966,
#     "stats":{"last_date_stored":"13251234600000000","values":["2210922"]},
#     "ui_visible":true}}}

# C:\Users\Nil\Downloads\test


# this works huge help
# https://github.com/operasoftware/operachromiumdriver/issues/72
options = webdriver.ChromeOptions()
opera_profile = r"C:\Users\Nil\Downloads\test"
options.add_argument('user-data-dir=' + opera_profile)
# options._binary_location = 'C:/Users/Nil/Downloads/operadriver_win64/operadriver.exe'
# driver = webdriver.Opera(executable_path="operadriver.exe",options=options)
driver = webdriver.Opera(
  executable_path="C:/Users/Nil/Downloads/operadriver_win64/operadriver.exe", 
  options=options)


options = webdriver.ChromeOptions()
opera_profile = r"C:\Users\Nil\AppData\Roaming\Opera Software\Opera Stable"
options.add_argument('user-data-dir=' + opera_profile)
# options._binary_location = 'C:/Users/Nil/Downloads/operadriver_win64/operadriver.exe'
# driver = webdriver.Opera(executable_path="operadriver.exe",options=options)
driver = webdriver.Opera(
  executable_path="C:/Users/Nil/Downloads/operadriver_win64/operadriver.exe", 
  options=options)

driver.get('https://api.ipify.org/?format=json"')
html = driver.page_source
print(html)

driver.quit()
