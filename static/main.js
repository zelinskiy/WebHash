
function upload(){
    out = document.getElementById("out");
    out.innerHTML = "";
    var formData = new FormData();
    formData.append("file", document.getElementById("file").files[0]);
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/upload", false);
    xhr.send(formData);
    
    out.innerHTML = xhr.responseText;
}
