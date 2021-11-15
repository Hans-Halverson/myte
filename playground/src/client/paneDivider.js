import { useCallback, useRef } from "react";

export default function PaneDivider(props) {
  const prevXRef = useRef(null);
  const offsetRef = useRef(props.offset);
  offsetRef.current = props.offset;

  const markNewX = (newX) => {
    props.setOffset(offsetRef.current + (newX - prevXRef.current));
    prevXRef.current = newX;
  };

  const onMouseMove = (event) => {
    if (prevXRef.current === null) {
      return;
    }

    markNewX(event.screenX);
  };

  const onMouseUp = (event) => {
    markNewX(event.screenX);
    document.removeEventListener("mousemove", onMouseMove);
    document.removeEventListener("mouseup", onMouseUp);
  };

  const onMouseDown = (event) => {
    prevXRef.current = event.screenX;
    document.addEventListener("mousemove", onMouseMove);
    document.addEventListener("mouseup", onMouseUp);
  };

  return <div className="divider" onMouseDown={onMouseDown}></div>;
}
