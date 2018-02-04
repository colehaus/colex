import { props, withComponent } from 'skatejs';
import withPreact from '@skatejs/renderer-preact';
import preact from 'preact';
import { h } from 'preact';

class WithPreact extends withComponent(withPreact()) {
  static get props() {
    return {
      name: props.string
    };
  }
  render({ name }) {
    return <span>Jello, {name}!</span>;
  }
}

customElements.define('with-preact', WithPreact);
