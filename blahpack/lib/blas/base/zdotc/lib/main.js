

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zdotc = require( './zdotc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zdotc, 'ndarray', ndarray );


// EXPORTS //

module.exports = zdotc;
