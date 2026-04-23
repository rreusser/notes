
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztpcon = require( './ztpcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztpcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztpcon;
