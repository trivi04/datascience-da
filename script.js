// async function submitAnswer() {
//     const modelAnswer = localStorage.getItem("modelAnswer");
//     const studentAnswer = document.getElementById("studentAnswer").value;
  
//     const data = new URLSearchParams();
//     data.append("model_answer", modelAnswer);
//     data.append("student_answer", studentAnswer);
  
//     try {
//       const response = await fetch("http://localhost:8000/evaluate_answer", {
//         method: "POST",
//         headers: {
//           "Content-Type": "application/x-www-form-urlencoded"
//         },
//         body: data
//       });
  
//       const result = await response.json();
//       console.log("API Response:", result);
  
//       // Display score in a friendly way
//       const score = result?.scores?.simple_score || "N/A";
//       document.getElementById("resultOutput").innerHTML = `
//         <strong>Score:</strong> ${score} / 5<br>
//         <pre>${JSON.stringify(result, null, 2)}</pre>
//       `;
//     } catch (err) {
//       console.error("API error:", err);
//       alert("Failed to contact scoring server.");
//       document.getElementById("resultOutput").textContent = "Error contacting server.";
//     }
//   }
  
// Save question and answer in localStorage
function saveData() {
  // This function was previously used for local storage. We will replace its logic
  // with sending data to the backend API in the event listener below.
  }
  
  // Add event listener for the faculty form submission
  document.addEventListener('DOMContentLoaded', function() {
    if (window.location.pathname.includes("faculty.html")) {
      const facultyForm = document.querySelector('form'); // Assuming your faculty form is the first form on the page
      if (facultyForm) {
        facultyForm.addEventListener('submit', async function(event) {
          event.preventDefault(); // Prevent default form submission

          const question = document.getElementById('question').value;
          const modelAnswer = document.getElementById('modelAnswer').value;

          const data = new URLSearchParams();
          data.append("question", question);
          data.append("model_answer", modelAnswer);

          try {
              const response = await fetch("http://localhost:8000/save_question", {
                  method: "POST",
                  headers: {
                      "Content-Type": "application/x-www-form-urlencoded"
                  },
                  body: data
              });
              const result = await response.json();
              alert(result.message); // Display success message
          } catch (error) {
              console.error("Error saving question:", error);
              alert("Failed to save question.");
          }
        });
      }
    }
  });

  // Add event listener for the Add Student form submission
  document.addEventListener('DOMContentLoaded', function() {
    if (window.location.pathname.includes("faculty.html")) {
      const addStudentForm = document.querySelector('#addStudentForm'); // Assuming your Add Student form has this ID
      if (addStudentForm) { // Also check if the form exists on the current page
        addStudentForm.addEventListener('submit', async function(event) {
          event.preventDefault(); // Prevent default form submission

          const studentName = document.getElementById('studentName').value;

          const data = new URLSearchParams();
          data.append("student_name", studentName);

          try {
              const response = await fetch("http://localhost:8000/add_student", {
                  method: "POST",
                  headers: { "Content-Type": "application/x-www-form-urlencoded" },
                  body: data
              });
              const result = await response.json();
              alert(result.message); // Display success message
          } catch (error) {
              console.error("Error adding student:", error);
              alert("Failed to add student.");
          }
        });
      }
    }
  });
  
  // Add event listener for the student form submission
  document.addEventListener('DOMContentLoaded', function() {
    if (window.location.pathname.includes("student.html")) {
      const studentForm = document.querySelector('#studentAnswerForm'); // Assuming your student form has this ID
      if (studentForm) {
        studentForm.addEventListener('submit', async function(event) {
          console.log("Student form submitted"); // Debugging line
          event.preventDefault(); // Prevent default form submission

          const studentAnswer = document.getElementById('studentAnswer').value;
          const modelAnswer = localStorage.getItem("modelAnswer"); // We need the model answer. Assuming it's stored in localStorage when fetched.
          const studentName = document.getElementById("studentNameInput").value; // Get student name from input field

          if (!modelAnswer) {
            alert("Model answer not available. Cannot evaluate.");
            return;
          }

          const data = new URLSearchParams();
          data.append("model_answer", modelAnswer);
          data.append("student_answer", studentAnswer);
          console.log("Sending to evaluate_answer:", data.toString()); // Debugging line

          try {
            const res = await fetch("http://localhost:8000/evaluate_answer", {
            method: "POST",
            headers: { "Content-Type": "application/x-www-form-urlencoded" },
            body: data
          });

          const result = await res.json();
          console.log("Evaluation Result:", result); // Debugging line

          // Display scores
          const scoreDisplay = document.getElementById("score-display"); // Assuming you have an element with this ID
          if (scoreDisplay) {
            scoreDisplay.innerHTML = `
              <h3>Evaluation Results:</h3>
              <p>Simple Score: ${result.scores.simple_score}</p>
              <p>CFPD Score: ${result.scores.cfpd_score}</p>
            `;
            // You can also display other details from the result if needed
            console.log("Detailed Results:", result);
          }

          // Now, save the submission
          const submissionData = new URLSearchParams();
          submissionData.append("student_name", studentName);
          // You'll need the question text here. You might need to store it when fetching the question.
          const questionText = document.getElementById("question-text").innerText.replace("Question:", "").trim(); // Get question text from display element
          submissionData.append("question", questionText);
          submissionData.append("student_answer", studentAnswer);
          submissionData.append("scores", JSON.stringify(result.scores)); // Save the scores as a JSON string

          console.log("Sending to save_submission:", submissionData.toString()); // Debugging line
          const saveResponse = await fetch("http://localhost:8000/save_submission", {
            method: "POST",
            headers: { "Content-Type": "application/x-www-form-urlencoded" },
            body: submissionData
          });

          const saveResult = await saveResponse.json();
          console.log("Save Submission Result:", saveResult); // Debugging line

          // Optionally display a message confirming submission saved
          // alert(saveResult.message);
        } catch (err) {
          console.error("API error:", err);
          alert("Failed to contact scoring server.");
          const scoreDisplay = document.getElementById("score-display");
          if (scoreDisplay) {
            scoreDisplay.textContent = "Error contacting server.";
          }
        }

      });
      }
  });
  
  // Add JavaScript code that runs only on report.html
  window.onload = function() {
      if (window.location.pathname.includes('report.html')) {
          console.log("Loading report page..."); // Debugging
          fetch("http://localhost:8000/average_scores_by_student")
              .then(response => response.json())
              .then(data => {
                  console.log("Report data received:", data); // Debugging
                  const studentNames = Object.keys(data);
                  const averageScores = Object.values(data);

                  const ctx = document.getElementById('performanceChartCanvas').getContext('2d'); // Assuming you add a canvas with this ID

                  new Chart(ctx, {
                      type: 'bar',
                      data: {
                          labels: studentNames,
                          datasets: [{
                              label: 'Average Simple Score',
                              data: averageScores,
                              backgroundColor: 'rgba(75, 192, 192, 0.6)',
                              borderColor: 'rgba(75, 192, 192, 1)',
                              borderWidth: 1
                          }]
                      },
                      options: {
                          scales: {
                              y: { beginAtZero: true, max: 5 } // Assuming scores are out of 5
                          }
                      }
                  });
              })
              .catch(error => console.error("Error fetching average scores:", error));
      }
  };
      // Draw chart
      drawChart(result.scores.simple_score, result.scores.cfpd_score);
    } catch (err) {
      console.error("API error:", err);
      alert("Failed to contact scoring server.");
    }
  }
  
  // Draw chart
  function drawChart(simple, cfpd) {
    const ctx = document.getElementById("scoreChart").getContext("2d");
    new Chart(ctx, {
      type: "bar",
      data: {
        labels: ["Simple Score", "CFPD Score"],
        datasets: [{
          label: "Scoring Comparison",
          data: [simple, cfpd],
          backgroundColor: ["#3498db", "#2ecc71"]
        }]
      },
      options: {
        scales: { y: { beginAtZero: true, max: 5 } }
      }
    });
  }
  ;kljn
  // Function to download report (moved outside DOMContentLoaded)
    const records = JSON.parse(localStorage.getItem("submissions") || "[]");
    let csv = "Name,Simple Score,CFPD Score\n";
    records.forEach(r => {
      csv += `${r.name},${r.simple},${r.cfpd}\n`;
    });
  
    const blob = new Blob([csv], { type: "text/csv" });
    const link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download = "assessment_report.csv";
    link.click();
  }

  // Role-based access control
document.addEventListener('DOMContentLoaded', function () {
    const currentPage = window.location.pathname;

    // Check if the current page is student.html
    if (currentPage.includes("student.html")) {
      // Load question if student
      const questionDisplay = document.getElementById("question-text"); // Corrected ID based on student.html
      const scoreDisplay = document.getElementById("score-display"); // Ensure score display is available
      if (questionDisplay) {
        questionDisplay.innerHTML = "<strong>Loading question...</strong>"; // Loading message
        fetch("http://localhost:8000/get_question")
          .then(response => response.json())
          .then(data => {
            if (data && data.question) {
              questionDisplay.innerHTML = `<strong>Question:</strong> ${data.question}`;
              localStorage.setItem("modelAnswer", data.model_answer); // Store model answer
            } else {
              questionDisplay.innerHTML = "<strong>No question available at the moment.</strong>";
            }
          })
          .catch(error => {
            console.error("Error fetching question:", error);
            questionDisplay.innerHTML = "<strong>Error loading question.</strong>";
          })
          .finally(() => {
            // Clear loading message or indicate status if no question
            if (questionDisplay.innerHTML === "<strong>Loading question...</strong>") {
              questionDisplay.innerHTML = "<strong>Could not load question.</strong>";
            }
          });
      }
    }

    // Load submissions if faculty
    if (currentPage.includes("faculty.html")) {
      const submissionsList = document.getElementById("submissions-list"); // Assuming you have an element with this ID
      if (submissionsList) {
        fetch("http://localhost:8000/get_submissions")
          .then(response => response.json())
          .then(data => {
            if (data && Array.isArray(data)) {
              submissionsList.innerHTML = "<h3>Student Submissions:</h3>"; // Clear loading or previous content
              data.forEach(submission => {
                const submissionElement = document.createElement("div");
                submissionElement.classList.add("submission"); // Add a class for styling
                submissionElement.innerHTML = `
                  <h4>Student: ${submission.student_name}</h4>
                  <p><strong>Question:</strong> ${submission.question}</p>
                  <p><strong>Student Answer:</strong> ${submission.student_answer}</p>
                  <p><strong>Scores:</strong> Simple: ${submission.scores.simple_score}, CFPD: ${submission.scores.cfpd_score}</p>
                  <hr>
                `;
                submissionsList.appendChild(submissionElement);
              });
            } else {
              submissionsList.innerHTML = "<p>No student submissions available yet.</p>";
            }
          })
          .catch(error => {
            console.error("Error fetching submissions:", error);
            submissionsList.innerHTML = "<p>Error loading submissions.</p>";
          });
      }
    }

    // Load report if faculty
  });

  };
  