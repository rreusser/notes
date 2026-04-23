
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_hercond_x = require( './zla_hercond_x.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_hercond_x, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_hercond_x;
