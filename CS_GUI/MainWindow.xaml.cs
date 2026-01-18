using System.Collections.ObjectModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using static API;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public ObservableCollection<CalculationHistoryItem> History { get; }
            = new ObservableCollection<CalculationHistoryItem>();
        public MainWindow()
        {
            InitializeComponent();
            DataContext = this;
        }

        /// <summary>
        /// Adds a new entry to the calculation history.
        /// </summary>
        /// <param name="expression">Mathematical expression entered</param>
        /// <param name="result">Result of mathematical expression</param>
        private void AddToHistory(string expression, string result)
        {
            History.Insert(0, new CalculationHistoryItem
            {
                Expression = expression,
                Result = result
            });
        }
        private void TextBox_TextChanged(object sender, TextChangedEventArgs e)
        {

        }

        /// <summary>
        /// Handles the Click event of the Calculate button, evaluating the user input and updating the plot and history
        /// accordingly.
        /// </summary>
        /// <remarks>This method processes the expression entered by the user, displays the result, and
        /// updates the plot area if applicable. If an error occurs during evaluation, the error message is displayed to
        /// the user.</remarks>
        /// <param name="sender">The source of the event, typically the Calculate button.</param>
        /// <param name="e">The event data associated with the Click event.</param>
        private void buttonCalculate_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = ""; // clear error box text

                string inputText = inputBox.Text;
                API.clearPlotPoints();
                // send input text to interpreter
                string interpreterReturn = API.interpret(inputText);
                AddToHistory(inputText, interpreterReturn); // add expression and result to history
                var points = API.getPlotPoints();
                if (points.Any())
                {
                    int interpMode = API.getInterpolationMode();
                    PlotArea.Interpolation = interpMode == 0
                        ? PlottingArea.InterpolationMode.Linear
                        : PlottingArea.InterpolationMode.Spline;

                    double minX = points.Min(p => p.Item1);
                    double maxX = points.Max(p => p.Item1);
                    PlotArea.PlotFunction(points, minX, maxX);
                }
    

            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }

        }
        /// <summary>
        /// Handles the click event of the Clear History button and clears the history collection.
        /// </summary>
        /// <param name="sender">The source of the event, typically the Clear History button.</param>
        /// <param name="e">The event data associated with the button click.</param>
        private void ClearHistoryButton_Click(object sender, RoutedEventArgs e)
        {
            History.Clear();
        }

        /// <summary>
        /// Handles the click event of the Plot button to generate and display a plot of the user-defined function.
        /// </summary>
        /// <remarks>This method retrieves the function expression and plotting parameters from the user
        /// interface, passes this info to the interpreter which returns list of points, and updates the plot display accordingly. If an error occurs during
        /// processing, an error message is displayed to the user.</remarks>
        /// <param name="sender">The source of the event, typically the Plot button.</param>
        /// <param name="e">The event data associated with the click event.</param>
        private void buttonPlot_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string function = inputBox.Text;
                // strip leading "y ="
                string expr = GetCurrentExpression();

                double minX = MinXDoubleUpDown.Value ?? 0;
                double maxX = MaxXDoubleUpDown.Value ?? 0;
                double step = StepDoubleUpDown.Value ?? 1;

                // ask interpreter to compute points
                API.plotFunction(expr, minX, maxX, step);

                // read points from F# list
                var points = API.getPlotPoints();

                // hand points over to plotting area
                PlotArea.ResetAxes();
                PlotArea.PlotFunction(points, minX, maxX);
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        /// <summary>
        /// Retrieves the current mathematical expression entered by the user, excluding any preceding assignment or
        /// variable name.
        /// </summary>
        /// <returns>A string containing the current expression from the input box. If the input includes an assignment (e.g., "y = x^2")
        /// returns the portion after the equals sign; otherwise, returns the entire trimmed input.</returns>
        private string GetCurrentExpression()
        {
            string text = inputBox.Text;
            return text.Contains("=")
                ? text.Split('=')[1].Trim()
                : text.Trim();
        }

        /// <summary>
        /// Handles the Click event of the Derivative button to numerically compute the derivative of the current
        /// expression at a specified point.
        /// </summary>
        /// <remarks>This method retrieves the current mathematical expression and the value of <c>x₀</c>
        /// from the user interface, computes the numerical derivative at that point, and adds the result to the
        /// calculation history. If an error occurs during computation, an error message is displayed to the
        /// user.</remarks>
        /// <param name="sender">The source of the event, typically the Derivative button.</param>
        /// <param name="e">The event data associated with the Click event.</param>
        private void buttonDerivative_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double x0 = DerivativeX0UpDown.Value ?? 0.0;
                double stepSize = 1e-4;

                double value = API.differentiateNumeric(expr, x0, stepSize);

                AddToHistory($"f'({x0}) for {expr}", value.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        /// <summary>
        /// Handles the Click event of the Root button to compute the root of the current mathematical expression within
        /// a specified interval.
        /// </summary>
        /// <remarks>This method retrieves the current expression and interval endpoints from the user
        /// interface, then attempts to find a root using the bisection method. If the operation fails, an error message
        /// is displayed to the user.</remarks>
        /// <param name="sender">The source of the event, typically the Root button.</param>
        /// <param name="e">The event data associated with the click event.</param>
        private void buttonRoot_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double a = RootAUpDown.Value ?? 0.0;
                double b = RootBUpDown.Value ?? 0.0;

                double tolerance = 1e-6;
                int maxIterations = 100;

                double root = API.findRootBisection(expr, a, b, tolerance, maxIterations);

                AddToHistory($"Root in [{a}, {b}] for {expr}", root.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        /// <summary>
        /// Handles the Click event of the Integrate button, performing numerical integration of the current expression
        /// over the specified interval.
        /// </summary>
        /// <remarks>This method retrieves the current mathematical expression and the integration bounds
        /// from the user interface, computes the definite integral using the trapezoidal rule, and adds the result to
        /// the calculation history. If an error occurs during integration, an error message is displayed to the
        /// user.</remarks>
        /// <param name="sender">The source of the event, typically the Integrate button.</param>
        /// <param name="e">The event data associated with the button click.</param>
        private void buttonIntegrate_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double a = IntegralAUpDown.Value ?? 0.0;
                double b = IntegralBUpDown.Value ?? 0.0;

                int steps = 1000; // fixed resolution (might change)

                double area = API.integrateTrapezoidal(expr, a, b, steps);

                AddToHistory($"∫[{a}, {b}] {expr} dx", area.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }
        /// <summary>
        /// Handles the Click event of the Parse Tree button, generating and displaying the parse tree for the current
        /// input text.
        /// </summary>
        /// <remarks>If an error occurs while generating the parse tree, the error message is displayed to
        /// the user.</remarks>
        /// <param name="sender">The source of the event, typically the Parse Tree button.</param>
        /// <param name="e">The event data associated with the Click event.</param>
        private void buttonParseTree_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";
                parseTreeBox.Text = "";

                string inputText = inputBox.Text;
                string tree = API.getParseTreeString(inputText);
                parseTreeBox.Text = tree;
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        /// <summary>
        /// Handles the event when the selection changes in the interpolation mode combo box.
        /// </summary>
        /// <remarks>Updates the interpolation mode of the plot area based on the user's selection in
        /// the combo box. This method is intended to be used as an event handler for the <see
        /// cref="SelectionChangedEventHandler"/> event of a combo box that allows users to choose the interpolation
        /// mode for plotting.</remarks>
        /// <param name="sender">The source of the event, typically the interpolation combo box.</param>
        /// <param name="e">The event data that contains information about the selection change.</param>
        private void InterpolationCombo_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (!IsLoaded) return;

            switch (InterpolationCombo.SelectedIndex)
            {
                case 0:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Linear;
                    break;
                case 1:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Spline;
                    break;
                default:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Linear;
                    break;
            }
        }

        /// <summary>
        /// Handles the click event for the "Plot Tangent" button, calculating and plotting the tangent line to the
        /// current expression at a specified point.
        /// </summary>
        /// <remarks>This method evaluates the current mathematical expression at the specified <c>x</c>
        /// value, computes the numerical derivative at that point, and plots the resulting tangent line on the existing
        /// plot area. Any errors encountered during evaluation or plotting are displayed in the error box.</remarks>
        /// <param name="sender">The source of the event, typically the button that was clicked.</param>
        /// <param name="e">The event data associated with the click event.</param>
        private void buttonPlotTangent_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string expr = GetCurrentExpression();
                double x0 = DerivativeX0UpDown.Value ?? 0.0;

                double stepSize = 1e-4;

                // f(x0)
                double fx0 = API.evaluateExpression(expr, x0);

                // f'(x0)
                double slope = API.differentiateNumeric(expr, x0, stepSize);

                // plot tangent line on existing plot
                PlotArea.PlotTangentLine(
                    x0,
                    fx0,
                    slope,
                    MinXDoubleUpDown.Value ?? x0 - 5,
                    MaxXDoubleUpDown.Value ?? x0 + 5
                );
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        /// <summary>
        /// Handles the Click event of the Clear Tangents button and removes all tangent lines from the plot area.
        /// </summary>
        /// <param name="sender">The source of the event, typically the Clear Tangents button.</param>
        /// <param name="e">The event data associated with the Click event.</param>
        private void buttonClearTangents_Click(object sender, RoutedEventArgs e)
        {
            PlotArea.ClearTangents();
        }

        /// <summary>
        /// Handles the click event for the button that plots the definite integral of the current mathematical
        /// expression over a specified interval.
        /// </summary>
        /// <remarks>This method retrieves the current expression and integration bounds from the user
        /// interface, computes the definite integral using the trapezoidal rule, and displays both the calculated area
        /// and the corresponding plot. If an error occurs during the process, an error message is displayed to the
        /// user.</remarks>
        /// <param name="sender">The source of the event, typically the button that was clicked.</param>
        /// <param name="e">The event data associated with the click event.</param>
        private void buttonPlotIntegral_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string expr = GetCurrentExpression();
                double a = IntegralAUpDown.Value ?? 0.0;
                double b = IntegralBUpDown.Value ?? 0.0;

                if (a > b)
                {
                    double tmp = a;
                    a = b;
                    b = tmp;
                }

                int steps = 1000;

                // calculate area
                double area = API.integrateTrapezoidal(expr, a, b, steps);

                // show visual area
                double step = StepDoubleUpDown.Value ?? 0.01;

                API.clearPlotPoints();
                API.plotFunction(expr, a, b, step);
                var areaPoints = API.getPlotPoints();

                PlotArea.PlotIntegrationArea(
                    areaPoints,
                    a,
                    b,
                    area,
                    expr
                );
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        /// <summary>
        /// Handles the Click event of the Clear Integrals button and removes all integral markers from the plot area.
        /// </summary>
        /// <param name="sender">The source of the event, typically the Clear Integrals button.</param>
        /// <param name="e">The event data associated with the Click event.</param>
        private void buttonClearIntegrals_Click(object sender, RoutedEventArgs e)
        {
            PlotArea.ClearIntegrals();
     
        }

        /// <summary>
        /// Handles changes to the aspect ratio lock checkbox and updates the plot area accordingly.
        /// </summary>
        /// <remarks>When the aspect ratio lock is toggled, this method updates the plot area's aspect
        /// ratio setting and attempts to replot the current function using the latest parameters. If the plot cannot be
        /// updated (for example, due to invalid input), the error is silently ignored.</remarks>
        /// <param name="sender">The checkbox control that triggered the event. Its <see cref="CheckBox.IsChecked"/> value determines whether
        /// the plot area's aspect ratio is locked.</param>
        /// <param name="e">The event data associated with the checkbox state change.</param>
        private void AspectRatioCheckBox_Changed(object sender, RoutedEventArgs e)
        {
            PlotArea.LockAspectRatio =
                (sender as CheckBox)?.IsChecked ?? false;

            // replot current function if possible
            try
            {
                string expr = GetCurrentExpression();

                double minX = MinXDoubleUpDown.Value ?? 0;
                double maxX = MaxXDoubleUpDown.Value ?? 0;
                double step = StepDoubleUpDown.Value ?? 0.1;

                API.clearPlotPoints();
                API.plotFunction(expr, minX, maxX, step);

                var points = API.getPlotPoints();
                PlotArea.ResetAxes();
                PlotArea.PlotFunction(points, minX, maxX);
            }
            catch
            {
                // ignore if no valid plot yet
            }
        }


    }
}