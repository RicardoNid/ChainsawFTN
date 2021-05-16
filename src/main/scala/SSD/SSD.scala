package object SSD {

  import spinal.lib.bus.avalon._

  val interfaceConfig = AvalonMMConfig(
    addressWidth = 12,
    dataWidth = 16,
    burstCountWidth = 12,
    useByteEnable = false,
    useDebugAccess = false,
    useRead = true,
    useWrite = true,
    useResponse = false,
    useLock = false,
    useWaitRequestn = false,
    useReadDataValid = true,
    useBurstCount = true)
}
