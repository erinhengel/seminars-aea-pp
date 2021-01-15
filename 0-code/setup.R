# Set up document (ggplot defaults, knitr hooks, etc.)

knitr::opts_chunk$set(
  echo=FALSE,
  cache=FALSE,
  warning=FALSE
)

library(foreign)
library(knitr)
library(ggpubr)
library(kableExtra)
library(tidyverse)

# Plot theme.
theme <- theme_set(theme_light())
theme <- theme_update(
  plot.title=element_text(colour="gray25", hjust=0.5, size=8),
  axis.text.x=element_text(colour="gray25", size=8),
  axis.text.y=element_text(colour="gray25", size=8),
  axis.title.x=element_text(colour="gray25", size=8),
  axis.title.y=element_text(colour="gray25", size=8),
  legend.position="bottom",
  legend.title = element_blank(),
  legend.text=element_text(colour="gray25", size=7),
  panel.border=element_blank(),
  legend.background=element_rect(colour = "transparent", fill=NA),
  legend.key.size=unit(0.45, "cm")
)

ColourPalette <- c("#FF6B6B", "#556270", "#C7F464", "#4ECDC4", "#C44D58")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Hook for figures
knit_hooks$set(plot = function(x, options) {
  
  return(paste("\n\\begin{widefigure}\n",
               "\\centering",
               "\\includegraphics", ifelse(isTRUE(options$fig.manualwidth), paste("[width=", options$fig.width, "\\linewidth]", sep=""), "[width=\\linewidth]"), "{", opts_knit$get("base.url"), paste(x, collapse = "."), "}\n",
               "\n\\caption{", options$fig.cap, "}\n", "\\label{fig:", options$label, "}",
               ifelse(!is.null(options$fig.note), paste("\n\\begin{figurenotes}\n", options$fig.note, "\n\\end{figurenotes}\n"), ""),
               ifelse(!is.null(options$fig.source), paste("\n\\begin{figurenotes}[Source]\n", options$fig.source, "\n\\end{figurenotes}\n"), ""),
               "\n\\end{widefigure}\n",
               sep = '')
  )
})
knit_hooks$set(crop=hook_pdfcrop)
