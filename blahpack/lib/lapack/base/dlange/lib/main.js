

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlange = require( './dlange.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlange, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlange;
