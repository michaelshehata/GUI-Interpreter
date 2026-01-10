using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for TopMenu.xaml
    /// </summary>
    public partial class TopMenu : UserControl
    {
        public TopMenu()
        {
            InitializeComponent();
        }

        private void SyntaxHelpMenuClicked(object sender, RoutedEventArgs e)
        {
            SyntaxHelpWindow helpWindow = new SyntaxHelpWindow();
            helpWindow.Show();
        }

        private void ClearVariables_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                API.clearVariables();
                MessageBox.Show(
                    "All user defined variables have been cleared.\n\n" +
                    "Built in constants remain available",
                    "Variables Cleared",
                    MessageBoxButton.OK,
                    MessageBoxImage.Information
                );
            }
            catch (Exception ex)
            {
                MessageBox.Show(
                    $"Error clearing variables: {ex.Message}",
                    "Error",
                    MessageBoxButton.OK,
                    MessageBoxImage.Error
                );
            }
        }

        private void Exit_Click(object sender, RoutedEventArgs e)
        {
            var result = MessageBox.Show(
                "Are you sure you want to exit?",
                "Exit Application",
                MessageBoxButton.YesNo,
                MessageBoxImage.Question
            );

            if (result == MessageBoxResult.Yes)
            {
                Application.Current.Shutdown();
            }
        }

        private void Authors_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show(
                "Advanced Programming Project\n" +
                "CMP-6048A / CMP-7009A\n\n" +
                "Group 21\n\n" +
                "Team Members:\n" +
                "• Michael Shehata\n" +
                "• Ali Jamjoum\n" +
                "• Luke Wilson\n\n" +
                "School of Computing Sciences\n" +
                "University of East Anglia\n" +
                "January 2026",
                "About the Authors",
                MessageBoxButton.OK,
                MessageBoxImage.Information
            );
        }

        private void Version_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show(
                "Math Evaluator & Interpreter\n" +
                "Version 1.0\n\n" +
                "Features:\n" +
                "✓ Arithmetic expression evaluation (INT1)\n" +
                "✓ Variable assignment and usage (INT2)\n" +
                "✓ For loops with step control (INT3)\n" +
                "✓ Advanced mathematics (INT4):\n" +
                "  - Numerical differentiation\n" +
                "  - Numerical integration (trapezium rule)\n" +
                "  - Root finding (bisection method)\n" +
                "✓ Function plotting with interpolation (GUI2/GUI3)\n" +
                "✓ Parse tree visualization\n" +
                "✓ Built-in mathematical functions\n\n" +
                "Implementation:\n" +
                "• F# Interpreter\n" +
                "• C# WPF GUI\n" +
                "• OxyPlot for visualization\n\n" +
                "© 2026 Group 21",
                "Version Information",
                MessageBoxButton.OK,
                MessageBoxImage.Information
            );
        }
    }
}