# SETUP: Load the knitr library
library(knitr)

# SETUP: Load the knit.rmd.file function
knit.rmd.file <- function(input.path, output.path){
  VAR <- ""
  knitr::knit(input=input.path, output=output.path)
  # knitr::knit(input="RMD/FAA-Strategy/FAA_Explanation.Rmd", output="_posts/2016-05-16-FAA_Explanation.md")
  VAR <- "/"
  knitr::knit(input=input.path, output=output.path)
  #knitr::knit(input="RMD/FAA-Strategy/FAA_Explanation.Rmd", output="_posts/2016-05-16-FAA_Explanation.md")
}

# 1. Change knit function input directory to select the location of the RMarkdown file ("RMD/xxxxx/Name.Rmd")
# %%% Example: knit.rmd.file("RMD/FAA-Strategy/FAA_Explanation.Rmd", .....)

# 2. Change knit function output directory of the Markdown file ("_posts/YYYY-MM-DD-Name.md")
# %%% Example: knit.rmd.file(....., "_posts/2016-05-16-FAA_Explanation.md")

# 3. Within the .Rmd file, change the fig.path to paste0(VAR, 'public/images/YYYY-MM-DD-Name/')
# ***Note for 3. fig.path is an attribute within opts_chunk$set(fig.path=..., message=..., warning=..., echo=...)***
# %%% Example: opts_chunk$set(fig.path=paste0(VAR, 'public/images/2014-10-27-DE_and_Reverse/'), .....)

# 4. Add the following to the TOP of the .Rmd file: ***NEED THE LAYOUT: POST***
# ---
#  layout: post
#  title: "Name of Post"
# ---
# %%% Example:
# %%% ---
# %%%  layout: post
# %%%  title: "DE and Reverse DE"
# %%% ---

# 5. Run the knit.rmd.file function (Examples shown below)

knit.rmd.file("RMD/FAA-Strategy/FAA_Explanation.Rmd", "_posts/2016-05-16-FAA_Explanation.md")
knit.rmd.file("RMD/Markowitz - Ilya/2015_06_05_Markowitz.Rmd", "_posts/2015-06-05-Markowitz.md")
knit.rmd.file("RMD/Rotation/Rotation_Strategy.Rmd", "_posts/2015-05-18-Rotation_Strategy.md")
knit.rmd.file("RMD/Momentum Markowitz - SIT/Momentum_Markowitz_Post.Rmd", "_posts/2015-06-04-Momentum_Markowitz.md")
knit.rmd.file("RMD/COGS_to_Revenue/COGS_to_Revenue.Rmd", "_posts/2016-05-16-COGS_to_Revenue.md")
knit.rmd.file("RMD/DE_and_Reverse/DE_and_Reverse_DE.Rmd", "_posts/2014-10-27-DE_and_Reverse.md")
knit.rmd.file("RMD/Gerber_Statistic_and_Hierarchical_Risk_Parity/Gerber_Statistic_and_Hierarchical_Risk_Parity.Rmd", "_posts/2016-05-26-Gerber_Statistic_and_Hierarchical_Risk_Parity.md")
# 6. Push the changes to git