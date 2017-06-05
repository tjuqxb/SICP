;安全，虽然每次操作会同时触发两个serializer过程，并产生两个能接受参数的经过serializer语法糖处理过的互斥元P过程。（见下章）
;由于其中一个P过程不会被返回，所以不会接收参数(.args),所以mutex的acquire过程和release过程都不会触发，因此是安全的。