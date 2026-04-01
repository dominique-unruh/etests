import puppeteer from 'puppeteer-core';
import { execSync } from 'child_process';

const browser = await puppeteer.launch({
  executablePath: '/usr/bin/chromium',
  args: ['--no-sandbox', '--disable-setuid-sandbox'],
});

const page = await browser.newPage();

// Add this once, before page.goto()
page.on('console', msg => console.error(`[browser] ${msg.text()}`));

console.log("Opening /workdir/input.html");

await page.goto('file:///workdir/input.html', { waitUntil: 'networkidle0' });

console.log("Awaiting MathJax");

await page.evaluate(async () => {
  // if (window.MathJax?.startup?.promise) {
    await window.MathJax.startup.promise;
  // }
});

console.log("Producing PDF")

await page.pdf({
  path: '/workdir/output.tmp.pdf',
  format: 'A4',
  outline: true,
  printBackground: true,
  displayHeaderFooter: true,
  headerTemplate: "<div/>",
  footerTemplate: `
    <div style="width:100%; text-align:center; font-size:10px; color:#555; margin-bottom:0.5cm;">
      <span class="pageNumber"></span> / <span class="totalPages"></span>
    </div>
  `,
  margin: {
    top: '1.5cm',
    bottom: '1.5cm',
    left: '1.5cm',
    right: '1.5cm',
  },
});

await browser.close();
console.log('Produced /workdir/output.tmp.pdf');

console.log('Fixing links...');
execSync('python3 /app/fix_links.py /workdir/output.tmp.pdf /workdir/output.pdf file:///workdir/');
console.log('Done.');