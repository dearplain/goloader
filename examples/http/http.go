package main

import "net/http"

func main() {
	go panic(http.ListenAndServe(":2300", http.FileServer(http.Dir("."))))
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("Hello goloader!"))
	})
	panic(http.ListenAndServe(":9090", nil))
}
