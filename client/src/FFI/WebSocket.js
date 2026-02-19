export const createImpl = (url) => () => {
  return new WebSocket(url);
};

export const sendImpl = (ws) => (msg) => () => {
  ws.send(msg);
};

export const onMessageImpl = (ws) => (cb) => () => {
  ws.onmessage = (event) => {
    cb(event.data)();
  };
};

export const onOpenImpl = (ws) => (cb) => () => {
  ws.onopen = () => {
    cb();
  };
};

export const onCloseImpl = (ws) => (cb) => () => {
  ws.onclose = () => {
    cb();
  };
};

export const closeImpl = (ws) => () => {
  ws.close();
};
