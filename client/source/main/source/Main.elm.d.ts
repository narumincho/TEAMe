export namespace Elm {
  namespace Main {
    function init(args: {
      flags: Flags;
    }): {
      ports: Ports;
    };
  }
}

type Flags = {};

type Ports = {};

type SubForElmCmd<T> = {
  subscribe: (arg: (value: T) => void) => void;
};

type CmdForElmSub<T> = {
  send: (value: T) => void;
};
