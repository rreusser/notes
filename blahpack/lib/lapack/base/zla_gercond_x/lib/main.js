/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gercond_x = require( './zla_gercond_x.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gercond_x, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gercond_x;
