void plot_SAN12() {
    gStyle->SetOptStat(0);
    gStyle->SetTitleFontSize(0.04);

    // === Read the histogram data ===
    // The Fortran code outputs (X,Y) pairs, so we convert them into a TH1F.
    std::ifstream fin("histogram.dat");
    std::vector<double> xvals, yvals;
    double x, y;
    while (fin >> x >> y) {
        xvals.push_back(x);
        yvals.push_back(y);
    }
    fin.close();

    if (xvals.empty()) {
        std::cerr << "Error: histogram.dat not found or empty.\n";
        return;
    }

    // Determine histogram range and binning
    double xmin = xvals.front();
    double xmax = xvals.back();
    int nbins = xvals.size();

    // Create histogram and fill it
    TH1F* hdata = new TH1F("hdata", "SAN12 Fit;Channel;Counts", nbins, xmin, xmax);
    for (int i = 0; i < nbins; ++i)
        hdata->SetBinContent(i + 1, yvals[i]);

    hdata->SetLineColor(kBlack);
    hdata->SetLineWidth(2);
    hdata->SetFillColor(kGray);
    hdata->SetFillStyle(3004);

    // === Create a canvas ===
    auto c = new TCanvas("c", "SAN12 Fit", 1000, 700);
    hdata->Draw("HIST");  // draw as a histogram, not as points
    hdata->SetTitle("SAN12 Spectrum with Fits;Channel;Counts");

    // === Overlay total fit ===
    auto total = new TGraph("total_fit.dat");
    total->SetLineColor(kRed);
    total->SetLineWidth(3);
    total->Draw("L SAME");

    // === Overlay background line ===
    auto bg = new TGraph("background.dat");
    bg->SetLineColor(kGray + 2);
    bg->SetLineStyle(2);
    bg->SetLineWidth(2);
    bg->Draw("L SAME");

    // === Overlay individual peak fits ===
    int peakCount = 0;
    for (int j = 1; j <= 50; ++j) {  // up to 50 peaks
        TString fname = Form("peak_%d.dat", j);
        if (gSystem->AccessPathName(fname)) continue;
        auto gr = new TGraph(fname);
        gr->SetLineColor(kBlue + (j % 5));
        gr->SetLineWidth(2);
        gr->Draw("L SAME");
        peakCount++;
    }

    // === Legend ===
    auto leg = new TLegend(0.12, 0.65, 0.32, 0.85);
    leg->AddEntry(hdata, "Experimental Data", "f");
    leg->AddEntry(total, "Total Fit", "l");
    leg->AddEntry(bg, "Background", "l");
    if (peakCount > 0)
        leg->AddEntry((TObject*)0, Form("%d Individual Peaks", peakCount), "");
    leg->Draw();

    // === Adjust axis and save ===
    hdata->GetXaxis()->SetRangeUser(xmin, xmax); // full data range
    c->SaveAs("SAN12_plot.pdf");
    c->SaveAs("SAN12_plot.png");

    std::cout << "✅ Plot saved as SAN12_plot.pdf and SAN12_plot.png\n";
}
