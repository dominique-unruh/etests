
show :
	(sleep 10; xdg-open http://localhost:9000/preview/) &
	sbt "project webapp; run"
