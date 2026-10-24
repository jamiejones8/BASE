// Run from the repository root on macOS to refresh the shared Apple Color Emoji artwork.
import AppKit
for (glyph,name) in [("🤖","robot"),("🍀","clover")] {
 let bitmap = NSBitmapImageRep(bitmapDataPlanes:nil,pixelsWide:256,pixelsHigh:256,bitsPerSample:8,samplesPerPixel:4,hasAlpha:true,isPlanar:false,colorSpaceName:.deviceRGB,bytesPerRow:0,bitsPerPixel:0)!
 NSGraphicsContext.saveGraphicsState()
 NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep:bitmap)
 (glyph as NSString).draw(at:NSPoint(x:0,y:0),withAttributes:[.font:NSFont(name:"Apple Color Emoji",size:220)!])
 NSGraphicsContext.restoreGraphicsState()
 var x0=256,y0=256,x1=0,y1=0
 for y in 0..<256 { for x in 0..<256 { if bitmap.colorAt(x:x,y:y)!.alphaComponent > 0.01 { x0=min(x0,x);y0=min(y0,y);x1=max(x1,x);y1=max(y1,y) } } }
 let cropped = bitmap.cgImage!.cropping(to:CGRect(x:x0,y:y0,width:x1-x0+1,height:y1-y0+1))!
 let out=NSBitmapImageRep(cgImage:cropped)
 try out.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:"WallyApps/HittingApp/www/ump-\(name).png"))
}
