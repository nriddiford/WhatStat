[![WhatStat](https://img.shields.io/badge/launch-WhatStat-brightgreen.svg)](https://whatstat.shinyapps.io/WhatStat/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# WhatStat

A tool to parse and visualise your WhatsApp group chats.

[Click here](https://whatstat.shinyapps.io/WhatStat/) to run the shiny web app.

Please [e-mail](mailto:nickriddiford@gmail.com) me if you are experiencing problems with WhatStat.

## How to export your WhatsApp chat log

#### iPhone
Follow the steps outlined on the [WhatsApp FAQ page](https://faq.whatsapp.com/en/iphone/20888066/)
1. Open the WhatsApp conversation you want to export
2. Tap the contact's name or group subject in the navigation bar
3. Scroll to the bottom and tap "Export Chat"
4. Select "Attach without Media"
5. Select "Save to Phone" or, if you want to email the log, select the Mail app

#### Android
Follow the steps outlined on the [WhatsApp FAQ page](https://faq.whatsapp.com/en/android/23756533)
1. Open the chat for the individual or group
2. Tap the Menu button
3. Select More
4. Select "Email chat"
5. Select "Attach without Media"

## Install from GitHub

```
git clone https://github.com/nriddiford/WhatStat.git
```

Start an R session, and install package:

```
library(devtools)
install_github("nriddiford/WhatStat")
library(WhatStat)
```

## How to run in Shiny

[Click here](https://whatstat.shinyapps.io/WhatStat/) to run the shiny web app.
