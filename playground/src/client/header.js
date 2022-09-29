import { useRef, useState } from "react";

const Commands = require("../server/commands");

const PLAY_ICON_SVG = (
  <svg
    className="playIcon"
    version="1.1"
    id="Capa_1"
    x="0px"
    y="0px"
    viewBox="0 0 17.804 17.804"
  >
    <g>
      <g id="c98_play">
        <path d="M2.067,0.043C2.21-0.028,2.372-0.008,2.493,0.085l13.312,8.503c0.094,0.078,0.154,0.191,0.154,0.313    c0,0.12-0.061,0.237-0.154,0.314L2.492,17.717c-0.07,0.057-0.162,0.087-0.25,0.087l-0.176-0.04    c-0.136-0.065-0.222-0.207-0.222-0.361V0.402C1.844,0.25,1.93,0.107,2.067,0.043z" />
      </g>
      <g id="Capa_1_78_"></g>
    </g>
  </svg>
);

const GEAR_ICON_SVG = (
  <svg
    className="gearIcon"
    x="0px"
    y="0px"
    width="50"
    height="50"
    viewBox="0 0 50 50"
  >
    <path d="M47.16,21.221l-5.91-0.966c-0.346-1.186-0.819-2.326-1.411-3.405l3.45-4.917c0.279-0.397,0.231-0.938-0.112-1.282 l-3.889-3.887c-0.347-0.346-0.893-0.391-1.291-0.104l-4.843,3.481c-1.089-0.602-2.239-1.08-3.432-1.427l-1.031-5.886 C28.607,2.35,28.192,2,27.706,2h-5.5c-0.49,0-0.908,0.355-0.987,0.839l-0.956,5.854c-1.2,0.345-2.352,0.818-3.437,1.412l-4.83-3.45 c-0.399-0.285-0.942-0.239-1.289,0.106L6.82,10.648c-0.343,0.343-0.391,0.883-0.112,1.28l3.399,4.863 c-0.605,1.095-1.087,2.254-1.438,3.46l-5.831,0.971c-0.482,0.08-0.836,0.498-0.836,0.986v5.5c0,0.485,0.348,0.9,0.825,0.985 l5.831,1.034c0.349,1.203,0.831,2.362,1.438,3.46l-3.441,4.813c-0.284,0.397-0.239,0.942,0.106,1.289l3.888,3.891 c0.343,0.343,0.884,0.391,1.281,0.112l4.87-3.411c1.093,0.601,2.248,1.078,3.445,1.424l0.976,5.861C21.3,47.647,21.717,48,22.206,48 h5.5c0.485,0,0.9-0.348,0.984-0.825l1.045-5.89c1.199-0.353,2.348-0.833,3.43-1.435l4.905,3.441 c0.398,0.281,0.938,0.232,1.282-0.111l3.888-3.891c0.346-0.347,0.391-0.894,0.104-1.292l-3.498-4.857 c0.593-1.08,1.064-2.222,1.407-3.408l5.918-1.039c0.479-0.084,0.827-0.5,0.827-0.985v-5.5C47.999,21.718,47.644,21.3,47.16,21.221z M25,32c-3.866,0-7-3.134-7-7c0-3.866,3.134-7,7-7s7,3.134,7,7C32,28.866,28.866,32,25,32z"></path>
  </svg>
);

const CHEVRON_DOWN_SVG = (
  <svg className="chevronDownIcon" viewBox="4 4 16 16">
    <path d="M12,15.5a1,1,0,0,1-.71-.29l-4-4A1,1,0,1,1,8.71,9.79L12,13.1l3.3-3.18a1,1,0,1,1,1.38,1.44l-4,3.86A1,1,0,0,1,12,15.5Z" />
  </svg>
);

function RunButton(props) {
  return (
    <button className="headerButton" onClick={props.onClick}>
      <div className="headerButtonText">Run</div>
      {PLAY_ICON_SVG}
    </button>
  );
}

function DropdownButton(props) {
  const [isDropdownActive, setIsDropdownActive] = useState(false);
  const toggleRef = useRef();
  const dropdownRef = useRef();

  function onCloseDropdown() {
    setIsDropdownActive(false);
    document.removeEventListener("click", onCloseDropdown);
  }

  function onToggleDropdown(event) {
    if (isDropdownActive) {
      onCloseDropdown();
      return;
    }

    setIsDropdownActive(true);

    // Clicks outside the dropdown and its trigger will close the dropdown
    document.addEventListener("click", (event) => {
      if (
        dropdownRef.current != null &&
        dropdownRef.current.contains(event.target)
      ) {
        return;
      }

      onCloseDropdown();
    });
  }

  return (
    <div
      className={`dropdown ${props.className} ${
        isDropdownActive ? "is-active" : ""
      }`}
      ref={dropdownRef}
    >
      <div className="dropdown-trigger">
        <button
          className="headerButton"
          aria-haspopup="true"
          aria-controls={props.id}
          onClick={onToggleDropdown}
        >
          {props.renderButtonContents()}
        </button>
      </div>
      <div
        className="dropdown-menu headerDropdownMenu"
        id={props.id}
        role="menu"
      >
        <div className="dropdown-content headerDropdownContent">
          {props.renderDropdownContents(onCloseDropdown)}
        </div>
      </div>
    </div>
  );
}

function CommandSelector(props) {
  return (
    <DropdownButton
      id="command-selector"
      className="commandSelector"
      renderButtonContents={() => (
        <>
          <div className="headerButtonText">{props.command.label}</div>
          {CHEVRON_DOWN_SVG}
        </>
      )}
      renderDropdownContents={(onCloseDropdown) =>
        Object.values(Commands).map((command) => {
          const isSelected = command.id === props.command.id;

          return (
            <button
              key={command.id}
              className={`dropdown-item headerDropdownItem ${
                isSelected ? "headerDropdownSelectedItem" : ""
              }`}
              onClick={() => {
                props.onChangeCommand(command);
                onCloseDropdown();
              }}
            >
              <p className="headerDropdownItemTitle">{command.label}</p>
              <p className="headerDropdownItemBody">{command.description}</p>
            </button>
          );
        })
      }
    />
  );
}

function SettingsMenu(props) {
  function onToggleShouldOptimize() {
    props.onChangeSettings({
      ...props.settings,
      shouldOptimize: !props.settings.shouldOptimize,
    })
  }

  function onToggleShouldStyleTerminalOutput() {
    props.onChangeSettings({
      ...props.settings,
      shouldStyleTerminalOutput: !props.settings.shouldStyleTerminalOutput,
    });
  }

  return (
    <DropdownButton
      id="settings-menu"
      className="settingsMenu"
      renderButtonContents={() => (
        <>
          <div className="headerButtonText">Settings</div>
          {GEAR_ICON_SVG}
        </>
      )}
      renderDropdownContents={() => (
        <>
          <label className="checkbox headerDropdownCheckbox">
            <input
              type="checkbox"
              className="headerDropdownCheckboxInput"
              checked={props.settings.shouldOptimize}
              onChange={onToggleShouldOptimize}
            />
            Optimize
          </label>
          <label className="checkbox headerDropdownCheckbox">
            <input
              type="checkbox"
              className="headerDropdownCheckboxInput"
              checked={props.settings.shouldStyleTerminalOutput}
              onChange={onToggleShouldStyleTerminalOutput}
            />
            Style Terminal Output
          </label>
        </>
      )}
    />
  );
}

export default function Header(props) {
  return (
    <header className="header">
      <a className="headerBrand" href="#">
        <img className="headerLogo" src="resources/myte-logo.svg" />
        <div className="headerTitle">MYTE</div>
      </a>
      <RunButton onClick={props.onSubmit} />
      <CommandSelector
        command={props.settings.command}
        onChangeCommand={(command) =>
          props.onChangeSettings({ ...props.settings, command })
        }
      />
      <SettingsMenu
        settings={props.settings}
        onChangeSettings={props.onChangeSettings}
      />
    </header>
  );
}
