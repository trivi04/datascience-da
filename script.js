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
    const question = document.getElementById("question").value;
    const modelAnswer = document.getElementById("modelAnswer").value;
    // const studentName = document.getElementById("studentName").value;
    const studentName = document.getElementById("studentName").value;
    
    localStorage.setItem("question", question);
    localStorage.setItem("modelAnswer", modelAnswer);
    localStorage.setItem("studentName", studentName);
  
    alert("Question posted successfully!");
  }
  
  // Show question on student page
  window.onload = function () {
    const qEl = document.getElementById("questionDisplay");
    if (qEl) {
      const question = localStorage.getItem("question");
      qEl.innerHTML = `<strong>${question}</strong>`;
    }
  
    // Report Page
    const reportTable = document.getElementById("reportTable");
    if (reportTable) {
      const records = JSON.parse(localStorage.getItem("submissions") || "[]");
      let table = `<table border='1'><tr><th>Name</th><th>Simple</th><th>CFPD</th></tr>`;
      records.forEach(r => {
        table += `<tr><td>${r.name}</td><td>${r.simple}</td><td>${r.cfpd}</td></tr>`;
      });
      table += `</table>`;
      reportTable.innerHTML = table;
    }
  }
  
  // Submit answer and call R API
  async function submitAnswer() {
    const modelAnswer = localStorage.getItem("modelAnswer");
    const studentAnswer = document.getElementById("studentAnswer").value;
    // const studentName = localStorage.getItem("studentName");
    const studentName = localStorage.getItem("currentUser");

    const data = new URLSearchParams();
    data.append("model_answer", modelAnswer);
    data.append("student_answer", studentAnswer);
  
    try {
      const res = await fetch("http://localhost:8000/evaluate_answer", {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: data
      });
  
      const result = await res.json();
  
      // Show result
      document.getElementById("resultOutput").textContent = JSON.stringify(result, null, 2);
  
      // Save submission
      const submissions = JSON.parse(localStorage.getItem("submissions") || "[]");
      submissions.push({
        name: studentName,
        simple: result.scores.simple_score,
        cfpd: result.scores.cfpd_score
      });
      localStorage.setItem("submissions", JSON.stringify(submissions));
  
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
  // Export CSV
  function downloadReport() {
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
window.onload = function () {
    const role = localStorage.getItem("role");
    const currentPage = window.location.pathname;
  
    // Access control rules
    const accessRules = {
      "faculty.html": "faculty",
      "report.html": "faculty",
      "student.html": "student"
    };
  
    for (let page in accessRules) {
      if (currentPage.includes(page) && role !== accessRules[page]) {
        alert("Unauthorized access. Redirecting to login.");
        window.location.href = "index.html";
        return;
      }
    }
  
    // Load question if student
    const qEl = document.getElementById("questionDisplay");
    if (qEl) {
      const question = localStorage.getItem("question");
      qEl.innerHTML = `<strong>${question}</strong>`;
    }
  
    // Load report if faculty
    const reportTable = document.getElementById("reportTable");
    if (reportTable) {
      const records = JSON.parse(localStorage.getItem("submissions") || "[]");
      let table = `<table border='1'><tr><th>Name</th><th>Simple</th><th>CFPD</th></tr>`;
      records.forEach(r => {
        table += `<tr><td>${r.name}</td><td>${r.simple}</td><td>${r.cfpd}</td></tr>`;
      });
      table += `</table>`;
      reportTable.innerHTML = table;
    }
  };
  