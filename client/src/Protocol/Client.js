export const getWsUrl = () => {
  const proto = location.protocol === "https:" ? "wss:" : "ws:";
  return proto + "//" + location.host;
};
