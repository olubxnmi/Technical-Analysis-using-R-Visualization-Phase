# BDA400 Assignment 6: Technical Analysis Dashboard

## Student
Olubunmi Fabanwo

## Course
BDA400 Data Science Tools and Techniques

## Assignment
Assignment 6: Technical Analysis using R, Visualization Phase

## Project Description
This project implements an interactive R Shiny dashboard for stock portfolio visualization. The app fetches stock data from Yahoo Finance, displays stock price charts, overlays technical indicators, and annotates buy/sell signals based on moving average crossover rules.

## Main Features
- Fetches historical stock data using `quantmod`
- Supports Daily, Weekly, and Monthly time frames
- Provides Line, Area, and Candlestick-style chart views
- Overlays Short Moving Average and Long Moving Average
- Displays RSI and MACD charts in separate dashboard tabs
- Generates Buy and Sell signals using moving average crossover logic
- Shows signal table and data preview

## Required R Packages
```r
install.packages(c("shiny", "ggplot2", "quantmod", "dplyr", "zoo", "scales"))
```

## How to Run
1. Open RStudio.
2. Open the project folder.
3. Open `app.R`.
4. Run the app using:

```r
shiny::runApp()
```

or click **Run App** in RStudio.

## Suggested Test Inputs
- Symbol: AAPL
- Date range: last 12 months
- Time frame: Daily
- Chart type: Line or Candlestick
- Indicators: Short Moving Average, Long Moving Average, RSI, MACD

## Trading Rule
The dashboard uses a moving average crossover rule:

- Buy signal: Short moving average crosses above long moving average
- Sell signal: Short moving average crosses below long moving average
- Hold signal: No crossover event

## Repository Structure
```text
Fabanwo_Olubunmi_BDA400_Assignment6/
├── app.R
├── README.md
├── R/
│   └── helper_indicators.R
├── docs/
│   ├── Fabanwo_Olubunmi_BDA400_Assignment6_CoverPage.docx
│   └── Assignment6_Report.docx
└── screenshots/
    └── add_app_screenshots_here.txt
```

## AI Assistance Disclosure
I used ChatGPT (GPT-5.5, July 2026) for dashboard planning, Shiny code structure, trading rule logic, documentation, and debugging support. I verified the output by reviewing the R code, checking function logic, and preparing run instructions.
