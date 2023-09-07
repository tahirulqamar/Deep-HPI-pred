# Deep-HPI-pred: an R-shiny app for Deep Learning based Host-Pathogen Interaction Prediction

## Description
Deep-HPI-pred is a shiny application that integrates the detection and visualization of host-pathogen interaction networks. It is designed to help researchers better understand both model and non-model host-pathogen systems. The application offers functionalities for analyzing your own data or demo data, viewing the predicted interactions network, and conducting Gene Ontology (GO) enrichment analysis. 

In the backend, Deep-HPI-pred employs a multilayer perceptron (MLP) model from the 'neuralnet' package, which is trained on the normalized feature vectors. It then makes predictions using the trained model, and calculates performance metrics (accuracy, sensitivity, etc.) based on the test data.

Intriguingly, Deep-HPI-pred showcases an advanced feature - the integration of GO annotations, pulled directly from the UniProt website. This enhancement considerably broadens the application's capacity to predict host-pathogen interactions accurately. By employing GO annotations, our model is capable of identifying protein functions within the interaction network, which in turn illuminates the intricate relationships involved. With the GO annotations fully integrated, Deep-HPI-pred achieves heightened accuracy, fostering significant advancements in our understanding of host-pathogen interactions and paving the way for the identification of potential therapeutic targets.

Deep-HPI-pred app is freely accessible on its homepage: https://cbi.gxu.edu.cn/shiny-apps/Deep-HPI-pred/ 

## Usage Instructions

1. Start the Shiny application.
2. From the sidebar, you can choose to upload your own CSV data or use the demo data provided.
3. Select "Train and Predict" to initiate the interaction prediction using the provided data.
4. The "Results" tab will display the network visualization and predicted interaction data.
5. The "GO Analysis" tab allows you to conduct GO enrichment analysis and visualize it as a Network Graph. The network graph can be downloaded for further analysis.

## Contact Details

To provide feedback, ask questions, or for more insights:

<ul style="margin-bottom: 0; padding-left: 20px;">
  <li style="margin-bottom: 5px;">Dr. Muhammad Tahir ul Qamar (<a href="mailto:m.tahirulqamar@hotmail.com">m.tahirulqamar@hotmail.com</a>)</li>
  <li style="margin-bottom: 5px;">Ms. Fatima Noor (<a href="mailto:fatimanoor1122@yahoo.com">fatimanoor1122@yahoo.com</a>)</li>
  <li style="margin-bottom: 5px;">Dr. Xi-Tong Zhu (<a href="mailto:z724@qq.com">z724@qq.com</a>)</li>
  <li style="margin-bottom: 5px;">Dr. Yi-Xiong Guo (<a href="mailto:guoyixiong@webmail.hzau.edu.cn">guoyixiong@webmail.hzau.edu.cn</a>)</li>
  <li style="margin-bottom: 5px;">Prof. Ling-Ling Chen (<a href="mailto:llchen@gxu.edu.cn">llchen@gxu.edu.cn</a>)</li>
</ul>
