package main

import (
  "os"
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {

      url := os.Args[1]

    resp, err := http.Get(url)

    if err != nil {
        log.Fatal(err)
    }

    defer resp.Body.Close()

    body, err := ioutil.ReadAll(resp.Body)

    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(string(body))
}
