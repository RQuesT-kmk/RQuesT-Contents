

# About -------------------------------------------------------------------

#' this is from a slip of medical charge from Grand Hanthar hospital on 16 May 2026
#' Author: KMK
#' Date: 2026-05-16


# data --------------------------------------------------------------------
rm(list = ls())
description <- c("Consultation fees(regular)","OPD care and medical supplies","EMR service fee","Assessment fee")
net_amount_mmk <- c(12500, 26000, 6000, 12500)
df <- data.frame(description, net_amount_mmk)

# Libraries ---------------------------------------------------------------

pacman::p_load(dplyr,
               ggplot2,
               patchwork)

# Analysis ----------------------------------------------------------------

bar <- df |> 
    mutate(
        total = sum(net_amount_mmk),
        proportion = net_amount_mmk/sum(net_amount_mmk)
    ) |> 
    ggplot(aes(x = net_amount_mmk, 
               y = reorder(description, net_amount_mmk)))+
    geom_col(fill = "red")+
    geom_text(aes(x = net_amount_mmk, 
                  y = reorder(description, net_amount_mmk)),
              size = 6,
              label = net_amount_mmk,
              hjust = 1,
              colour = "gray10")+
    theme_minimal()+
    theme(axis.text.x = element_blank(),
          text = element_text(size = 18))+
    labs(title = "Hospital charges for a follow up patient in Yangon",
         x = "Net amount (MMK)",
         y = "Description of charges")


png(filename = "hospital_charge/net.amount.png",height = 720, width = 720)
bar
dev.off()

dodge <- df |> 
    mutate(
        total = sum(net_amount_mmk),
        proportion = net_amount_mmk/sum(net_amount_mmk)
    ) |> 
    ggplot(aes(x = proportion, 
               y = reorder(description, proportion),
               fill = description))+
    geom_col()+
    geom_text(aes(x = proportion, 
                  y = reorder(description, proportion),
                  label = scales::percent(proportion)),
              size = 6,
              hjust = -0.5,
              colour = "gray10")+
    scale_x_continuous(limits = c(0,1),
                       breaks = c(0,1), 
                       labels = scales::percent_format())+
    scale_fill_brewer(type = "qual", palette = 6, name = "Description of charges")+
    theme_bw()+
    theme(legend.position = "",
          text = element_text(size = 16))+
    labs(title = "",
         x = "Percentage of total charge",
         y = "Description of charges")
png(filename = "hospital_charge/prop.bar.png",height = 720, width = 720)
dodge
dev.off()

fill <- df |> 
    mutate(
        total = sum(net_amount_mmk),
        proportion = round(net_amount_mmk/sum(net_amount_mmk),2),
    ) |> 
    ggplot(aes(x = 1,
               y = proportion,
               fill = description))+
    geom_col(position = "fill", width = 0.75)+
    scale_y_continuous(limits = c(0,1),
                       breaks = c(0,1), 
                       labels = scales::percent_format(),
                       name = "Proportion of total charge")+
    scale_x_continuous(limits = c(0,2))+
    scale_fill_brewer(type = "qual", palette = 6, name = "Description of charges")+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = -30)),
          axis.ticks = element_blank(),
          legend.position = "",
          text = element_text(size = 16))+
    labs(title = "")
png(filename = "hospital_charge/prop.stacked.bar.png",height = 720, width = 720)
fill
dev.off()


patch <- dodge + fill + plot_annotation(title = "Proportion of fees charged by a private hospital in Yangon",
                                        theme = theme(plot.title = element_text(size = 16)))
png(filename = "hospital_charge/prop.charge.png",height = 720, width = 720)
patch
dev.off()

pie <- df |> 
    mutate(
        total = sum(net_amount_mmk),
        proportion = round(net_amount_mmk/sum(net_amount_mmk),2)
    ) |> 
    ggplot(aes(x = 1,
               y = proportion,
               fill = description =="Consultation fees(regular)"))+
    geom_col(width = 2)+
    coord_polar(theta = "y")+
    scale_fill_manual(
        values = c("TRUE" = "red", "FALSE" = "gold3"),
        labels = c("TRUE" = "Doctors", "FALSE" = "Private hospital"),
        name = "Fees charged by:"
    ) +
    theme_bw()+
    theme(text = element_text(size = 16),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())+
    labs(title = "Only 22% of the total bill is charged by the doctors")
png(filename = "hospital_charge/prop.pie.png",height = 720, width = 720)
pie
dev.off()

