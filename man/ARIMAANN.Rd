\name{ARIMAANN}
\alias{ARIMAANN}
\title{
ARIMA-ANN hybrid model fitting
}

\description{
The ARIMAANN function fit ARIMA-ANN hybrid model for time series data.
}

\usage{
ARIMAANN(data,h)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.}
\item{h}{
The forecast horizon.
}
}

\details{
This package allows you to fit the ARIMA-ANN hybrid model.
}

\value{
  \item{Test_Result}{Checking the suitability of data for hybrid modelling}
  \item{ARIMA coefficients}{Coefficients of the fitted ARIMA }
  \item{pvalues }{pvalues of the fitted ARIMA model}
  \item{ANN Summary}{Summary of the fitted ANN model on residuals obtained from
  the fitted ARIMA model}
  \item{MAPE}{Mean Absolute Percentage Error (MAPE) of the fitted hybrid model}
  \item{MSE}{Mean Square Error (MSE) of fitted hybrid model}
  \item{fitted }{Fitted values of hybrid model}
   \item{forecasted.values}{h step ahead forecasted values employing hybrid
   model}
}

\references{
Zhang, G. P.Time series forecasting using a hybrid ARIMA and neural network model
Neurocomputing, 50 (2003), pp. 159-175.

}
\author{
Ramasubramanian V., Mrinmoy Ray
}

\seealso{
auto.arima, nnetar
}

\examples{
data=lynx
ARIMAANN(data,5)
}

\keyword{ ARIMA-ANN }

