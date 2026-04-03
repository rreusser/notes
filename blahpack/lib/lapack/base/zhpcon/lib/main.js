
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpcon = require( './zhpcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpcon;
